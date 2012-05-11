/**
 * Partially implemented D lexer. I have no plans to continue working on this.
 *
 * Example:
 * ----
    auto source = Source(import("filename.d"));
    Token token;
    do
    {
        token = lex(source);
        write(token.raw);
    } while(token.type != Type.EOF);
 * ----
 * LICENSE: Boost 1.0
 * AUTHOR: Simen Endsjø <simendsjo@gmail.com>
 * */
module simendsjo.dc.lexer;

// Here be dragons!

import std.exception;
import std.stdio;
import core.vararg;
import std.range, std.conv, std.ascii, std.string;
import std.file;
import std.traits;
import std.algorithm;
import std.array;

enum Type : ushort
{
    _typeNotSet, // represents an internal lexer error
    invalid,
    identifier,

    // basic types
    void_, bool_, char_, wchar_, dchar_, auto_, int8, uint8, int16, uint16,
    int32, uint32, int64, uint64, float32, float64, float80,

    enum_, struct_, union_, interface_, class_, alias_, function_, delegate_,
    mixin_, override_,

    private_, protected_, package_, public_, extern_,

    static_, final_, const_, immutable_, abstract_,
    in_, out_, inout_, ref_, lazy_,

    debug_, version_, deprecated_,

    module_, import_,

    if_, else_, do_, while_, for_, switch_, case_, default_, break_, continue_,
    with_, synchronized_, return_, goto_, throw_, try_, catch_, finally_, foreach_,
    foreach_reverse_, scope_,

    body_, invariant_,
    unittest_,

    macro_,

    at,
    pure_, nothrow_,
    shared_, gshared,

    // special tokens
    FILE, LINE, EOF,
    TIME, TIMESTAMP, DATE,
    VENDOR, VERSION,

    xor, xorAssign, pow, powAssign,
    shiftLeft, shiftRight,

    // brackets
    lparen,     rparen,
    lbracket,   rbracket,
    lcurly,     rcurly,

    // literals
    boolv,
    int8v,      uint8v,
    int16v,     uint16v,
    int32v,     uint32v,
    int64v,     uint64v,
    float32v,   float64v, float80v,
    char8v, char16v, char32v,
    stringv, wstringv, dstringv,

    div, divAssign,
    and, andAssign, and2,
    or, orAssign, or2,
    minus, minusAssign, decrement,
    plus, plusAssign, increment,
    dot, range, dot3,
    question,
    comma,
    colon,
    dollar,
    star,
    percent,
    hash,
    semicolon,
    tilde,

    // comments
    lineComment, blockComment, codeComment,
    ddocLineComment, ddocBlockComment, ddocCodeComment,

    assign, equals, goesTo,
    greater, greaterEquals,
    less, lessEquals,

    exclaim, // !
    space, // ' '
    tab, // \t
    lineFeed, // \n
    carriageReturn, // \r
}

shared(Type[string]) stringToken;
shared(Type[string]) keywords;
shared(string[Type]) tokenString;
shared static this()
{
    scope(exit)
    {
        stringToken = cast(shared(Type[string]))stringToken.rehash;
        keywords    = cast(shared(Type[string]))keywords.rehash;
        tokenString = cast(shared(string[Type]))tokenString.rehash;
    }

    void add(string tok, Type type, bool isKeyword = false)
    {
        tokenString[type]   = tok;
        stringToken[tok]    = type;
        if(isKeyword)
            keywords[tok]   = type;
    }

    add(" ", Type.space);
    /+
    add("//", Type.lineComment);
    add("/*", Type.blockStartComment); add("*/", Type.blockEndComment);
    add("/+", Type.codeStartComment);  add("+/", Type.codeEndComment);
    add("///", Type.ddocLineComment);
    add("/**", Type.ddocBlockStartComment); add("*/", Type.ddocBlockEndComment);
    add("/++", Type.ddocCodeStartComment);  add("+/", Type.ddocCodeEndComment);
    +/
    add(";", Type.semicolon);
    add("\r", Type.carriageReturn);
    add("\n", Type.lineFeed);

    add("@", Type.at);
    add("^", Type.xor);
    add("^^", Type.pow);
    add("^^=", Type.powAssign);

    add("module", Type.module_, true);
    add("int", Type.int32, true);
    add("if", Type.if_, true);

    add("void", Type.void_, true);
    add("bool", Type.bool_, true);
    add("char", Type.char_, true);
    add("wchar", Type.wchar_, true);
    add("dchar", Type.dchar_, true);
    add("auto", Type.auto_, true);

    add("byte", Type.int8, true);
    add("ubyte", Type.uint8, true);
    add("short", Type.int16, true);
    add("ushort", Type.uint16, true);
    add("int", Type.int32, true);
    add("uint", Type.uint32, true);
    add("long", Type.int64, true);
    add("ulong", Type.uint64, true);
    add("float", Type.float32, true);
    add("double", Type.float64, true);
    add("real", Type.float80, true);

    add("enum", Type.enum_, true);
    add("struct", Type.struct_, true);
    add("union", Type.union_, true);
    add("interface", Type.interface_, true);
    add("class", Type.class_, true);
    add("alias", Type.alias_, true);
    add("function", Type.function_, true);
    add("delegate", Type.delegate_, true);
    add("mixin", Type.mixin_, true);
    add("override", Type.override_, true);

    add("private", Type.private_, true);
    add("protected", Type.protected_, true);
    add("package", Type.package_, true);
    add("public", Type.public_, true);
    add("extern", Type.extern_, true);

    add("static", Type.static_, true);
    add("final", Type.final_, true);
    add("abstract", Type.abstract_, true);

    add("in", Type.in_, true);
    add("out", Type.out_, true);
    add("ref", Type.ref_, true);
    add("inout", Type.inout_, true);
    add("lazy", Type.lazy_, true);

    add("debug", Type.debug_, true);
    add("version", Type.version_, true);
    add("deprecated", Type.deprecated_, true);

    add("module", Type.module_, true);
    add("import", Type.import_, true);

    add("if", Type.if_, true);
    add("else", Type.else_, true);
    add("do", Type.do_, true);
    add("while", Type.while_, true);
    add("for", Type.for_, true);
    add("switch", Type.switch_, true);
    add("case", Type.case_, true);
    add("default", Type.default_, true);
    add("break", Type.break_, true);
    add("continue_", Type.continue_, true);
    add("with", Type.with_, true);
    add("synchronized", Type.synchronized_, true);
    add("return", Type.return_, true);
    add("goto", Type.goto_, true);
    add("throw", Type.throw_, true);
    add("try", Type.try_, true);
    add("catch", Type.catch_, true);
    add("finally", Type.finally_, true);
    add("foreach", Type.foreach_, true);
    add("foreach_reverse", Type.foreach_reverse_, true);
    add("scope", Type.scope_, true);

    add("body", Type.body_, true);
    add("invariant", Type.invariant_, true);
    add("macro", Type.macro_, true);

    add("pure", Type.pure_, true);
    add("nothrow", Type.nothrow_, true);

    add("const", Type.const_, true);
    add("immutable", Type.immutable_, true);
    add("shared", Type.shared_, true);
    add("__gshared", Type.gshared, true);

    add("__FILE__", Type.FILE, true);
    add("__LINE__", Type.LINE, true);
    add("__DATE__", Type.DATE, true);
    add("__TIME__", Type.TIME, true);
    add("__TIMESTAMP__", Type.TIMESTAMP, true);
    add("__VENDOR__", Type.VENDOR, true);
    add("__VERSION__", Type.VERSION, true);
    add("__EOF__", Type.EOF, true);

    add("unittest", Type.unittest_, true);

}

struct Location
{
private:
    size_t line = -1;
    size_t column = -1;

    invariant()
    {
        assert(line > 0);
        assert(column > 0);
    }
}

struct LocationRange
{
    Location start;
    Location end;

    invariant()
    {
        assert(end.line >= start.line);
        // Can only point forward
        assert(end.line == start.line && end.column >= start.column);
    }
}

bool fitsIn(U, T)(T value)
{
    return cast(U)value == value;
}

Type smallestTypeOf(T)(T value)
{
    if(fitsIn!(bool)(value))
        return Type.boolv;
    else if(fitsIn!(byte)(value))
        return Type.int8v;
    else if(fitsIn!(ubyte)(value))
        return Type.uint8v;
    else if(fitsIn!(short)(value))
        return Type.int16v;
    else if(fitsIn!(ushort)(value))
        return Type.uint16v;
    else if(fitsIn!(int)(value))
        return Type.int32v;
    else if(fitsIn!(uint)(value))
        return Type.uint32v;
    else if(fitsIn!(long)(value))
        return Type.int64v;
    else if(fitsIn!(ulong)(value))
        return Type.uint64v;
    else if(fitsIn!(float)(value))
        return Type.float32v;
    else if(fitsIn!(double)(value))
        return Type.float64v;
    else if(fitsIn!(real)(value))
        return Type.float80v;
    assert(0);
}

template dtypeOf(Type type)
{
    static if(type == Type.boolv)
        alias bool dtypeOf;
    else static if(type == Type.int8v)
        alias byte dtypeOf;
    else static if(type == Type.uint8v)
        alias ubyte dtypeOf;
    else static if(type == Type.int16v)
        alias short dtypeOf;
    else static if(type == Type.uint16v)
        alias ushort dtypeOf;
    else static if(type == Type.int32v)
        alias int dtypeOf;
    else static if(type == Type.uint32v)
        alias uint dtypeOf;
    else static if(type == Type.int64v)
        alias long dtypeOf;
    else static if(type == Type.uint64v)
        alias ulong dtypeOf;
    else static if(type == Type.float32v)
        alias float dtypeOf;
    else static if(type == Type.float64v)
        alias double dtypeOf;
    else static if(type == Type.char8v)
        alias char dtypeOf;
    else static if(type == Type.char16v)
        alias wchar dtypeOf;
    else static if(type == Type.char32v)
        alias dchar dtypeOf;
    else static if(type == Type.stringv)
        alias string dtypeOf;
    else static if(type == Type.wstringv)
        alias wstring dtypeOf;
    else static if(type == Type.dstringv)
        alias dstring dtypeOf;
    else
        static assert(0, text("Literal type '", to!string(type), "' has no matching D type"));
}

bool isFloatingPointLiteral(Type type)
{
    return type == Type.float32v
        || type == Type.float64v
        || type == Type.float80v;
}

bool isFloatingPointType(Type type)
{
    return type == Type.float32
        || type == Type.float64
        || type == Type.float80;
}

bool isSignedIntegerType(Type type)
{
    return type == Type.int8
        || type == Type.int16
        || type == Type.int32
        || type == Type.int64;
}

bool isUnsignedIntegerType(Type type)
{
    return type == Type.uint8
        || type == Type.uint16
        || type == Type.uint32
        || type == Type.uint64;
}

bool isIntegralType(Type type)
{
    return type.isSignedIntegerType() || type.isUnsignedIntegerType();
}

bool isSignedIntegerLiteral(Type type)
{
    return type == Type.int8v
        || type == Type.int16v
        || type == Type.int32v
        || type == Type.int64v;
}

bool isUnsignedIntegerLiteral(Type type)
{
    return type == Type.uint8v
        || type == Type.uint16v
        || type == Type.uint32v
        || type == Type.uint64v;
}

bool isIntegerLiteral(Type type)
{
    return type.isSignedIntegerLiteral() || type.isUnsignedIntegerLiteral();
}

bool isCharType(Type type)
{
    return type == Type.char_
        || type == Type.wchar_
        || type == Type.dchar_;
}

bool isCharLiteral(Type type)
{
    return type == Type.char8v
        || type == Type.char16v
        || type == Type.char32v;
}

bool isDBuiltinType(Type type)
{
    return type == Type.bool_
        || type.isIntegralType()
        || type.isFloatingPointType()
        || type.isIntegralType();
}

bool isWhitespace(Type type)
{
    return type == Type.space
        || type == Type.tab;
}

template tokenTypeOf(T)
{
    static if(is(T == bool))
        enum tokenTypeOf = Type.boolv;
    else static if(is(T == byte))
        enum tokenTypeOf = Type.int8v;
    else static if(is(T == ubyte))
        enum tokenTypeOf = Type.uint8v;
    else static if(is(T == short))
        enum tokenTypeOf = Type.int16v;
    else static if(is(T == ushort))
        enum tokenTypeOf = Type.uint16v;
    else static if(is(T == int))
        enum tokenTypeOf = Type.int32v;
    else static if(is(T == uint))
        enum tokenTypeOf = Type.uint32v;
    else static if(is(T == long))
        enum tokenTypeOf = Type.int64v;
    else static if(is(T == ulong))
        enum tokenTypeOf = Type.uint64v;
    else static if(is(T == float))
        enum tokenTypeOf = Type.float32v;
    else static if(is(T == double))
        enum tokenTypeOf = Type.float64v;
    else static if(is(T == real))
        enum tokenTypeOf = Type.float80v;
    else static assert(0, "no corresponding token type");
}

// Creates property for Token literal values embedding an assert
// to make sure we only use the correct literal type
template VPropertyString(Type type)
{
    enum vstr = to!string(type);
    alias dtypeOf!type dtype;
    enum dtypestr = dtype.stringof;

    // getter
    enum VPropertyString =
        // getter
        dtypestr~` `~vstr~`() nothrow pure const @safe {`
            `assert(type == Type.`~vstr~`);`
            `return _`~vstr~`;}`
        // setter
       `void `~vstr~`(`~dtypestr~` value) nothrow pure @safe {`
        `assert(type == Type.`~vstr~`);`
        `_`~vstr~` = value; }`;
}
mixin template VProperty(Type type)
{
    enum vstr = to!string(type);
    alias dtypeOf!type dtype;
    enum dtypestr = dtype.stringof;

    @property
    {
        mixin(VPropertyString!(type));
    }
}

struct Token
{
    Type type;
    LocationRange location;
    /// The raw unproccessed text as found in the source file for this location range
    string raw;
    /// A D reserved keyword
    bool isKeyword;

    // values (literal)
    union
    {
        bool       _boolv;
        byte       _int8v;
        ubyte     _uint8v;
        short     _int16v;
        ushort   _uint16v;
        int       _int32v;
        uint     _uint32v;
        long      _int64v;
        ulong    _uint64v;

        float   _float32v;
        double  _float64v;

        char      _char8v;
        wchar    _char16v;
        dchar    _char32v;

        string   _stringv;
        wstring _wstringv;
        dstring _dstringv;
    }

    mixin VProperty!(Type.boolv);

    mixin VProperty!(Type.int8v);
    mixin VProperty!(Type.uint8v);
    mixin VProperty!(Type.int16v);
    mixin VProperty!(Type.uint16v);
    mixin VProperty!(Type.int32v);
    mixin VProperty!(Type.uint32v);
    mixin VProperty!(Type.int64v);
    mixin VProperty!(Type.uint64v);

    mixin VProperty!(Type.float32v);
    mixin VProperty!(Type.float64v);

    mixin VProperty!(Type.char8v);
    mixin VProperty!(Type.char16v);
    mixin VProperty!(Type.char32v);

    mixin VProperty!(Type.stringv);
    mixin VProperty!(Type.wstringv);
    mixin VProperty!(Type.dstringv);

    void setValue(Type type, T = dtypeOf!type)(T value) nothrow pure @safe
    {
        this.type = type;
        // This is a hack as we cannot have to! in a pure function even if it's only evaluated at compile time
        enum vstr = stringOf!type;
        mixin(vstr~` = cast(typeof(`~vstr~`))value;`);
        mixin(`assert(`~vstr~` == value);`);
    }

    void setValue(T)(T value) nothrow pure @safe
    {
        setValue!(tokenTypeOf!T)(value);
    }
}

// DMD bug 6169 - pull 652
// This is a hack as we cannot have to! in a pure function even if it's only evaluated at compile time
template stringOf(alias value)
{
    enum stringOf = to!string(value);
}

enum IssueType
{
    debug_,
    trace,
    info,
    hint,
    warning,
    error,
    fatal,
}

struct Issue
{
    IssueType       type;
    LocationRange   location;
    string          message;
    string          file;
    size_t          line;

    this(IssueType type, LocationRange location, string message,
            string file = __FILE__, size_t line = __LINE__) pure nothrow @safe
    {
        this.type       = type;
        this.location   = location;
        this.message    = message;

        this.file       = file;
        this.line       = line;
    }
}

enum SourceContext
{
    code,
    ddoc
}

struct Source
{
private:
    string source;
    string rest;

    Location    location;
    size_t      pos;
    Issue[]     issues;

    SourceContext context = SourceContext.code;

public:
    this(string source) pure nothrow @safe
    {
        this(source, Location(1, 1));
    }

    this(string source, Location location) pure nothrow @safe
    {
        this(source, location, 0);
    }

    this(string source, Location location, size_t pos) const pure @safe
    {
        this.source = source;
        this.location = location;
        this.pos = 0;
        this.rest = source[pos .. $];
    }

    bool matches(string txt) const pure nothrow @safe
    {
        for(size_t i; i < txt.length; i++)
            if(peekN(i) != txt[i]) return false;
        return true;
    }

    bool matchesAny(Args ...)(Args txts) const pure nothrow @safe
    {
        foreach(txt; txts)
            if(matches(txt)) return true;
        return false;
    }

    @property bool isEOF() const pure nothrow @safe
    {
        return empty || matchesAny("\0", "\x1A"); // FIXME: is this correct? what if it's in a string or comment?
    }

    @property bool isNewline() const nothrow pure @safe
    {
        return matches("\r\n") || matches("\n");
    }

    @property bool isEOL() const nothrow pure @safe
    {
        return isEOF || isNewline;
    }

    void consumeAny(Args ...)(Args txts) pure @safe
    in
    {
        assert(matchesAny(txts));
    }
    body
    {
        foreach(txt; txts)
        {
            if(consumeIfMatch(txt))
                return;
        }
        assert(0, "no texts exist to consume");
    }

    void consume(string txt) pure @safe
    in
    {
        assert(matches(txt));
    }
    body
    {
        for(int i; i < txt.length; i++)
        {
            assert(front == txt[i]);
            popFront();
        }
    }

    bool consumeIfMatch(string txt) pure @safe
    {
        if(matches(txt))
        {
            consume(txt);
            return true;
        }
        return false;
    }

    bool consumeIfMatchAny(Args ...)(Args txts)
    {
        foreach(txt; txts)
        {
            if(consumeIfMatch(txt))
                return true;
        }
        return false;
    }

    string sliceFrom(Source marked) const pure nothrow @safe
    in
    {
        assert(source == marked.source, "cannot select from Source - not the same source");
        assert(pos >= marked.pos);
        assert(pos <= source.length);
    }
    body
    {
        return source[marked.pos .. pos];
    }

    dchar peek() const pure nothrow @safe
    {
        return peekN(1);
    }

    dchar peek2() const pure nothrow @safe
    {
        return peekN(2);
    }

    dchar peekN(size_t n) const pure nothrow @safe
    in
    {
        assert(n >= 0);
    }
    body
    {
        if(pos+n >= source.length)
            return '\0';
        else
            return source[pos+n]; // TODO: decode?!
    }

    // cannot be const as don't take a copy of issues
    Source mark() pure @safe
    {
        auto result = Source(source, location);
        result.pos = pos;
        result.rest = rest;
        result.issues = issues;
        return result;
    }

    void popFront() pure @safe
    in
    {
        assert(!rest.empty, "cannot popFront on empty source");
    }
    body
    {
        switch(rest.front)
        {
            case '\n':
                location.line++;
                goto case; // reset column on nix/bsd
            case '\r':
                location.column = 1;
                break;
            default:
                location.column++;
                break;
        }
        rest.popFront();
        ++pos;
    }

    @property dchar front() const pure @safe
    {
        if(pos >= source.length)
            return '\0';
        return rest.front;
    }

    /**
     * Returns true if the entire source text has been iterated. This does not
     * check for any EOF markers in the codeComment
     * */
    @property bool empty() const pure nothrow @safe
    {
        // empty(T)(T[]) might throw, so we'll roll our own to use nothrow
        return rest.length == 0;
    }

    void error(string message, string file = __FILE__, size_t line = __LINE__) pure nothrow @safe
    {
        error(this.location, message, file, line);
    }

    void error(Location location, string message, string file = __FILE__, size_t line = __LINE__) pure nothrow @safe
    {
        error(LocationRange(this.location, location), message, file, line);
    }

    void error(LocationRange location, string message, string file = __FILE__, size_t line = __LINE__) pure nothrow @safe
    {
        issues ~= Issue(IssueType.error, location, message, file, line);
    }

    const pure nothrow @safe invariant()
    {
        assert(pos >= 0, "position cannot be negative");
        assert(pos <= source.length, "internal position exceeds source length");
    }
}

dtypeOf!(type) convToNumber(Type type)(string num)
{
    return to!(dtypeOf!(type))(num);
}


void lexHexLiteral(ref Source source, ref Token token, int num=0) @trusted
in
{
    assert(num >= 0, "cannot lex a negative amount of hex literals");
    assert(num%2 == 0, "hex literals comes in pairs");
}
body
{
    auto start = source.mark();
    Location location = token.location.start = token.location.end = source.location;
    ulong value;
    int numParsed; // number of parsed characters
    bool success;
    while(true)
    {
        assert(!source.isEOF, "source empty while lexing hex literal");
        location.column++;

        if(source.front == '_')
        {
            if(numParsed == 0)
            {
                source.error(location, "Hex literals cannot start with underscore.");
                token.type = Type.invalid;
                break;
            }
            else if(numParsed%2 != 0)
            {
                source.error(location, format("An underscore can only be used between groups of two hex characters."));
                token.type = Type.invalid;
                break;
            }
            else if(source.peek() == '\0')
            {
                source.error(location, "A hex literal cannot end in an underscore.");
                token.type = Type.invalid;
                break;
            }
            else if(numParsed%2 != 0 && !isHexDigit(source.front))
            {
                source.error(location, format("Expected hex character, got '%s'.", source.front));
                token.type = Type.invalid;
                break;
            }
            else
            {
                source.popFront();
                continue;
            }
            assert(0, "_ is front, but no condition is met!");
        }
        else if(!source.front.isHexDigit())
        {
            source.error(location, format("Expected hex character, got '%s'.", source.front));
            token.type = Type.invalid;
            break;
        }

        // Add hex value - Stolen from the dmd frontend
        assert(source.front.isHexDigit());
        auto ch = source.front;
        if(isDigit(ch))
            ch -= '0';
        else if(isLower(ch))
            ch -= ('a' - 10);
        else
            ch -= ('A' - 10);
        assert(cast(ubyte)ch >= 0 && cast(ubyte)ch <= 16);
        value = (value*16) + cast(ubyte)ch;
        ++numParsed;
        source.popFront();

        // Are we done or cannot proceed?
        bool finished = (numParsed >= num && num > 0)
            || (!source.isEOF && numParsed%2 == 0 && num == 0 && !source.front.isHexDigit());
        if(finished)
        {
            //TODO: peek to see if the next character is hex or _ - illegal!
            success = true;
            break;
        }
        else if(source.isEOF)
        {
            if(numParsed%2 != 0)
            {
                token.type = Type.invalid;
                source.error(location, "Expected hex character, got EOF.");
            }
            success = true;
            break;
        }
    }

    // Set value on token
    bool validData = numParsed > 0 && numParsed%2 == 0;
    if(success && validData)
    {
        if(numParsed <= 2)
            token.setValue!(Type.uint8v)(value);
        else if(numParsed <= 4)
            token.setValue!(Type.uint16v)(value);
        else if(numParsed <= 8)
            token.setValue!(Type.uint32v)(value);
        else if(numParsed <= 16)
            token.setValue!(Type.uint64v)(value);
        else
            assert(0, text("numParsed == ", numParsed));
    }

    // Enough hex digits according to type?
    bool tooFew = num > 0 && numParsed < num;
    if(tooFew)
    {
        switch(num)
        {
            case 0:
                break;
            case 2:
                source.error(text(`\x hex escape needs 2 hex characters, got `, numParsed, "."));
                token.type = Type.invalid;
                break;
            case 4:
                source.error(text(`\u hex escape needs 4 hex characters, got `, numParsed, "."));
                token.type = Type.invalid;
                break;
            case 8:
                source.error(text(`\U hex escape needs 8 hex characters, got `, numParsed, "."));
                token.type = Type.invalid;
                break;
            default:
                assert(0, text("num = ", num));
        }
    }

}

void lexHexEscape(ref Source source, ref Token token)
in
{
    assert(source.matchesAny("x", "u", "U"));
}
body
{
    int ndigits;
    switch(source.front)
    {
        case 'x': ndigits = 2; goto Lhex;
        case 'u': ndigits = 4; goto Lhex;
        case 'U': ndigits = 8; goto Lhex;
        Lhex:
            assert(ndigits > 0 && ndigits%2 == 0);
            source.popFront();
            source.lexHexLiteral(token, ndigits);
            break;
        default: assert(0, "Invalid hex escape start");
    }
}

void lexEscape(ref Source source, ref Token token)
in
{
    assert(source.front == '\\');
}
out
{
    assert(token.type == Type.char8v || token.type == Type.invalid);
}
body
{
    char ch = '!';
    source.popFront(); // \
    switch(source.front)
    {
        // FIXME: What about octal digits?
        // FIXME: What about named character entities?
        // FIXME: What about 0x1A and EOF?

        case 'x': case 'u': case 'U':
            source.lexHexEscape(token);
            // lexHexEscape sets the type as integer, so we need to change the type
            token.type = Type.char8v;
            assert(cast(char)token.int8v == token.char8v);
            break;

        case '\'':  ch = '\''; goto LsetValue;
        case '"':   ch = '\"'; goto LsetValue;
        case '?':   ch = '\?'; goto LsetValue;
        case '\\':  ch = '\\'; goto LsetValue;
        case 'a':   ch = '\a'; goto LsetValue;
        case 'b':   ch = '\b'; goto LsetValue;
        case 'f':   ch = '\f'; goto LsetValue;
        case 'n':   ch = '\n'; goto LsetValue;
        case 'r':   ch = '\r'; goto LsetValue;
        case 't':   ch = '\t'; goto LsetValue;
        case 'v':   ch = '\v'; goto LsetValue;
        case '0':   ch = '\0'; goto LsetValue;
        LsetValue:
            assert(ch != '!', "escape char not set");
            token.setValue!(Type.char8v)(ch);
            source.popFront(); // ch
            break;

        default:
            assert(ch == '!', "escape char set");
            source.error(text("Cannot lex character escape: invalid char escape '", source.front, "'"));
            token.type = Type.invalid;
            source.popFront();
            break;
    }
}

void lexChar(ref Source source, ref Token token)
in
{
    assert(source.front == '\'',
            text("expected ' to start char literal, got '", source.front, "'"));
}
out
{
    assert(token.type == Type.invalid || token.type == Type.char8v);
}
body
{
    source.popFront(); // '
    switch(source.front)
    {
        case '\\':
            source.lexEscape(token);
            break; // lexEscape should have popped

        case '\n':
            token.location.end.line++;
            goto case; // nix and bsd resets column too
        case '\r':
            token.location.end.column = 1;
            goto LPop;
        case '\0':
            goto LPop;
        case '\'':
            source.error("No character or unterminated '.");
            goto LPop;
        default:
            token.location.end.column++;
            goto LPop;

        LPop:
            source.popFront();
            break;
    }

    if(source.front != '\'')
    {
        source.error(text("Expected ' to end character literal, got '", source.front, "'."));
        // TODO: read to next ' or EOF?
    }
    else
    {
        token.type = Type.char8v;
        source.popFront(); // '
    }
}

void lexNumber(ref Source source, ref Token token)
in
{
    assert(!source.isEOF);
    switch(source.front)
    {
        case '-':
        case '+':
        case '0': .. case '9':
            break;
        default:
            // text() is impure
            assert(0, "current character: '" ~ to!char(source.front) ~ "' is not a number");
    }
}
body
{
    auto start = source.mark();
    bool neg = source.consumeIfMatch("-");

    if(source.consumeIfMatchAny("0b", "0B"))
    {
        source.lexBinaryLiteral(token);
    }
    else if(source.consumeIfMatchAny("0x", "0X"))
    {
        source.lexHexLiteral(token);
    }
    else
    {
        auto consumeDigits = () { while(source.front.isDigit()) source.popFront(); };
        if(source.front == '0' && source.peek() >= '0' && source.peek() <= '7')
        {
            source.error("Octal literals not supported. Use octal!()");
            token.type = Type.invalid;
            consumeDigits();
        }
        else
        {
            if(source.front == '.' && source.peek().isDigit() /* not UFCS */) // floating point
            {
                source.popFront(); // '.'
                consumeDigits();
            }
            else // integer
            {
                consumeDigits();
            }

            auto value = to!int(source.sliceFrom(start));
            token.setValue(value);
        }
    }
    // Get correct type
    // Convert number
    // Set location
    // Update source location
}

bool isValidBinaryLiteral(dchar ch)
{
    return ch == '0' || ch == '1' || ch == '_';
}

void lexBinaryLiteral(ref Source source, ref Token token)
in
{
    assert(source.front.isValidBinaryLiteral());
}
body
{
    auto app = appender!(char[])();
    for(; source.front.isValidBinaryLiteral(); source.popFront())
    {
        if(source.front != '_')
            app.put(source.front);
    }
    auto value = to!int(app.data, 2); // FIXME: Choose smallest type
    token.setValue(value);
}

void lexWysiwygString(ref Source source, ref Token token)
in
{
    assert(source.front == 'r' || source.front == '`');
}
out
{
    // lexString should set the type as it checks for postfix
    assert(token.type == Type._typeNotSet || token.type == Type.invalid);
}
body
{
    // find character to end literal
    if(source.front == 'r')
    {
        source.popFront();
        if(source.front != '"')
        {
            source.error(text(`Expected '"' following 'r' wysiwyg string, got '`, source.front, "'"));
            token.type = Type.invalid;
            return;
        }
        assert(source.front == '"');
    }
    auto end = source.front;
    assert(end == '"' || end == '`');
    source.popFront(); // skip start character

    auto data = appender!(char[])();
    while(source.front != end)
    {
        if(source.isEOF)
        {
            break;
        }
        else if(source.front == end)
        {
            break;
        }
        else if(source.front == '\\' && source.peek() == end)
        {
            source.popFront(); // \
            assert(source.front == end);
        }

        data.put(source.front);
        source.popFront();
    }

    if(source.front == end)
    {
        token.stringv = data.data.idup;
        source.popFront(); // skip end
    }
    else if(source.isEOF)
    {
        source.error(text("Cannot lex wysiwyg string: Expected '", end, "', got EOF."));
        token.type = Type.invalid;
    }
}

void lexHexString(ref Source source, ref Token token)
{
    assert(source.front == 'x');
    source.popFront();
    assert(source.front == '"');
    assert(0, "not implemented");
}

/*
 * q"IDENT
 * somestring
 * IDENT"
 * ->
 * "somestring
 * " // ending newline included
 * */
void lexIdentifierDelimitedString(ref Source source, ref Token token)
in
{
    assert(source.front.isValidIdentifierStart());
}
body
{
    // find ident
    auto identStart = source.mark();
    source.popFront();
    string ident;
    while(true)
    {
        if(source.isEOF)
        {
            source.error("Cannot lex delimited string:"
                    " Expected valid identifier character, but got EOF");
            token.type = Type.invalid;
            return;
        }
        else if(source.isNewline)
        {
            ident = source.sliceFrom(identStart);
            break;
        }
        else if(!source.front.isValidIdentifier())
        {
            source.error(text("Cannot lex delimited string:"
                    " Expected valid identifier character, but got '", source.front));
            token.type = Type.invalid;
            return;
        }
        source.popFront();
    }
    assert(ident && ident.length, "delimited ident not found");
    assert(!source.isEOF);
    assert(source.isNewline, "should stop at newline or return early when error is found");

    source.popFront(); // the beginning newline shouldn't be a part of the string
    auto start = source.mark();

    // read string until ending ident is found
    auto identEnd =  ident ~ "\"";
    auto nixEnd   =   "\n" ~ identEnd; // nix newline
    auto winEnd   = "\r\n" ~ identEnd; // windows newline
    while(!(source.isEOF || source.front == '"' || source.matchesAny(winEnd, nixEnd)))
        source.popFront();
    if(source.isEOF || source.front == '"')
    {
        source.error(text("Cannot lex delimited string: Didn't find ending \\n", ident, "\""));
        token.type = Type.invalid;
    }
    else
    {
        assert(source.isNewline);
        source.consumeAny("\r\n", "\n"); // ending newline is part of the string
        token.stringv = source.sliceFrom(start);
        source.consume(identEnd);
    }
}

/**
 * Returns: true if ch is any of [, (, < or {
 */
bool isOpeningDelimiter(in dchar ch) pure nothrow @safe
{
    switch(ch)
    {
        case '[': case '(': case '<': case '{': return true;
        default: return false;
    }
    assert(0);
}

/**
 * Returns: true if ch is any of ], ), > or }
 */
bool isClosingDelimiter(in dchar ch) pure nothrow @safe
{
    switch(ch)
    {
        case ']': case ')': case '>': case '}': return true;
        default: return false;
    }
    assert(0);
}

/**
 * Returns: The matching opening or closing delimiter for the symbol. Ie. ch=='(' returns ')'
 */
dchar getMatchingDelimiter(in dchar ch) pure nothrow @safe
in
{
    assert(ch.isOpeningDelimiter() || ch.isClosingDelimiter());
}
out(result)
{
    assert(result.isOpeningDelimiter() || result.isClosingDelimiter());
}
body
{
    switch(ch)
    {
        case '[': return ']';
        case '(': return ')';
        case '<': return '>';
        case '{': return '}';

        case ']': return '[';
        case ')': return '(';
        case '>': return '<';
        case '}': return '{';

        default: assert(0, "not a delimiter with matching");
    }
    assert(0);
}

void lexNestedDelimitedString(ref Source source, ref Token token)
in
{
    assert(source.front.isOpeningDelimiter());
}
out
{
    // lexString should set the type
    assert(token.type == Type._typeNotSet || token.type == Type.invalid);
}
body
{
    dchar open  = source.front;
    dchar close = open.getMatchingDelimiter();
    assert(close.isClosingDelimiter());

    source.popFront(); // opening delimiter
    auto start = source.mark();
    int depth = 1;
    do
    {
        if(source.front == '"')
        {
            source.error(text("Unmatched '", start, "'"));
            token.type = Type.invalid;
            return;
        }
        else if(source.front == open)
            ++depth;
        else if(source.front == close)
            --depth;

        if(depth > 0)
            source.popFront(); // don't pop ending delimiter as we don't want it in token.stringv
        assert(depth >= 0);
    } while(depth > 0);
    assert(source.front == close);

    if(source.peek() == '"')
    {
        auto data = source.sliceFrom(start);
        token.stringv = data;
        source.popFront(); // closing delimiter
        source.popFront(); // "
    }
    else
    {
        source.error(text(`Cannot lex delimited string:`
                    `Expected " following closing delimiter, but got '`, source.front, "'"));
        token.type = Type.invalid;
    }
}

void lexDelimitedString(ref Source source, ref Token token)
in
{
    assert(source.matches(`q"`));
}
out
{
    // lexString should set the type
    assert(token.type == Type._typeNotSet || token.type == Type.invalid);
}
body
{
    source.consume(`q"`);

    if(source.front.isOpeningDelimiter()) // nested delimiter
    {
        source.lexNestedDelimitedString(token);
    }
    else if(source.front.isValidIdentifierStart()) // identifier delimiter
    {
        source.lexIdentifierDelimitedString(token);
    }
    else // some single char delimiter
    {
        auto delim = source.front;
        source.popFront(); // delimiter is not part of the string
        auto start = source.mark();

        while(!(source.isEOF || source.front == delim)) source.popFront();
        if(source.isEOF)
        {
            source.error(text("Cannot lex delimited string: Didnt find ending '", delim, "'\""));
            token.type = Type.invalid;
        }
        else
        {
            assert(source.front == delim);
            token.stringv = source.sliceFrom(start);
            source.popFront(); // delimiter is not part of the string
            assert(source.front == '"');
            source.popFront();
        }
    }
}

void lexTokenString(ref Source source, ref Token token)
in
{
    assert(source.matches(`q{`));
}
out
{
    // lexString should set the type
    assert(token.type == Type._typeNotSet || token.type == Type.invalid);
}
body
{
    source.consume("q{");
    auto start = source.mark();
    int depth = 1;
    do
    {
        if(source.isEOF)
        {
            source.error("Cannot lex token string: Expected token or }, got EOF");
            token.type = Type.invalid;
            return;
        }
        else if(source.front == '{')
        {
            ++depth;
        }
        else if(source.front == '}')
        {
            --depth;
        }

        if(depth > 0)
            source.popFront(); // don't pop } as we don't want to include it in our token.stringv
        assert(depth >= 0);
    } while(depth > 0);
    assert(source.front == '}');
    token.stringv = source.sliceFrom(start);
    source.popFront();

    // token strings must be lexical valid, so let's lex the result and check for errors
    auto strSource = Source(token.stringv);
    while(lex(strSource).type != Type.EOF) {}
    // FIXME: Bug or spec?
    assert(strSource.empty, "EOF found in token string instead of at the end - is this really a bug? Or allowed according to spec?");

    auto errors = strSource.issues.filter!((a) => a.type == IssueType.error)().array();
    if(errors.length) // move errors over copy errors
    {
        errors.map!((e) => source.issues ~= e)();
        token.type = Type.invalid;
    }
}

void lexDoubleQuotedString(ref Source source, ref Token token)
in
{
    assert(source.front == '"',
            text("Expected \" to start double quoted string, got '", source.front, "'"));
}
out
{
    // if we successfully parse a string,
    // lexString should take care of setting the correct type
    assert(token.type == Type._typeNotSet
            || token.type == Type.invalid);
}
body
{
    // TODO: implement copy on write for the data in case it doesn't contain any escape literals?
    //       The appender is very slow..
    auto data = appender!(char[])();

    source.popFront(); // "
    while(!(source.isEOF || source.front == '"'))
    {
        if(source.front == '\\')
        {
            Token esc;
            source.lexEscape(esc);
            if(esc.type == Type.invalid)
                return; // FIXME: Try to recover? We don't know where or what failed. Could inspect issues..
            assert(esc.type == Type.char8v);
            data.put(esc.char8v);
        }
        else
        {
            data.put(source.front);
            source.popFront();
        }
    }

    if(source.isEOF)
    {
        source.error(`Cannot lex double quoted string: Expected character or '"', got EOF`);
        token.type = Type.invalid;
    }
    else
    {
        assert(source.front == '"');
        source.popFront(); // "
        token.stringv = data.data.idup;
    }
}

void lexString(ref Source source, ref Token token)
in
{
    assert(source.matchesAny(`"`, "q", "`", "x", "r"));
}
out
{
    assert(token.type == Type.invalid || token.type == Type.stringv ||
            token.type == Type.wstringv || token.type == Type.dstringv);
}
body
{
    // check for optional string postfix
    bool checkForPostfix = true;
    switch(source.front)
    {
        case '"':
            source.lexDoubleQuotedString(token);
            break;
        case 'q':
            switch(source.peek())
            {
                case '"':
                    source.lexDelimitedString(token);
                    break;
                case '{':
                    source.lexTokenString(token);
                    break;
                default:
                    source.error(text("Cannot lex delimited string: Expected \", ` or {, got '", source.peek(), "'."));
                    token.type = Type.invalid;
                    break;
            }
            checkForPostfix = false;
            break;
        case 'x':
            if(source.peek() == '"')
            {
                source.lexHexString(token);
            }
            else
            {
                source.error(text(`Cannot lex hex string: Expected ", got '`, source.peek(), "'."));
                token.type = Type.invalid;
            }
            break;
        case 'r':
            if(source.peek() == '"') // regular wysiwyg string
            {
                goto case;
            }
            else
            {
                source.error(text(`Cannot lex wysiwyg string: Expected ", got '`, source.peek(), "'."));
                token.type = Type.invalid;
                break;
            }
            assert(0);
        case '`':
            source.lexWysiwygString(token);
            break;
        default:
            assert(0, text("Cannot lex string. Unknown string start character '", source.front, "'"));
    }
    assert(token.type == Type.invalid || token.type == Type._typeNotSet,
            "the sub-lexstring functions should not set the type");

    if(token.type == Type.invalid)
        return;

    // optional string postfix
    if(!source.isEOF && checkForPostfix)
    {
        switch(source.front)
        {
            case 'w':
                source.popFront();
                token.type = Type.wstringv;
                break;
            case 'd':
                source.popFront();
                token.type = Type.dstringv;
                break;
            case 'c':
                source.popFront();
                goto default;
            default:
                token.type = Type.stringv;
                break;
        }
    }
    else
        token.type = Type.stringv;
}

bool isValidIdentifierStart(dchar ch)
{
    switch(ch)
    {
        case '_':
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
            return true;
        default:
            return false;
    }
    assert(0, "invalid identifier start");
}

bool isValidIdentifier(dchar ch)
{
    switch(ch)
    {
        case '_':
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '0': .. case '9':
            return true;
        default:
            return false;
    }
    assert(0, "invalid identifier");
}

bool isValidIdentifier(string ident)
{
    if(!ident[0].isValidIdentifierStart())
        return false;

    if(ident.length > 1)
    {
        foreach(ch; ident[1..$])
        {
            if(!ch.isValidIdentifier())
                return false;
        }
    }
    return true;
}

void lexIdentifier(ref Source source, ref Token token)
in
{
    assert(source.front.isValidIdentifierStart());
}
body
{
    auto start = source.mark();

    while(source.peek().isValidIdentifier())
            source.popFront();
    source.popFront();

    auto ident = source.sliceFrom(start);
    with(token)
    {
        stringv = ident;
        type = keywords.get(ident, Type.identifier);
        if(type != Type.identifier)
            isKeyword = true;
    }
}

void lexRegularComment(ref Source source, ref Token token)
in
{
    if(source.matchesAny("/++", "/**")) // could be ddoc
        assert(source.matchesAny("/++/", "/**/")); // nope - fooled!
    else
        assert(source.matchesAny("/+", "/*", "//"));
}
body
{
    source.consumeAny("//", "/*", "/+");
    auto start = source.mark();
    if(token.type == Type.lineComment)
    {
        while(!source.isEOL) source.popFront();
        token.stringv = source.sliceFrom(start);
    }
    else if(token.type == Type.blockComment)
    {
        while(!(source.isEOF || source.matches("*/"))) source.popFront();
        token.stringv = source.sliceFrom(start);
        if(!source.consumeIfMatch("*/"))
            source.error(`Expected "*/", got EOF.`);
    }
    else if(token.type == Type.codeComment)
    {
        auto level = 1;
        while(!source.isEOF && level > 0)
        {
            if(source.matches("+/"))
            {
                --level;
                level > 0 && source.consume("+/");
            }
            else if(source.consumeIfMatch("/+"))
                ++level;
            else
                source.popFront();
            assert(level >= 0);
        }

        if(level == 0)
        {
            token.stringv = source.sliceFrom(start);
            source.consume("+/");
        }
        else
            source.error(`Expected "+/", got EOF.`);
    }
    else assert(0);
}

void lexDdocComment(ref Source source, ref Token token)
in
{
    assert(!source.matchesAny("/++/", "/**/"));
    assert(source.matchesAny("/++", "/**", "///"));
}
body
{
    // FIXME: gobble up everything for now to focus on other parts
    source.consumeAny("///", "/**", "/++");
    auto start = source.mark();
    if(token.type == Type.lineComment)
    {
        while(!source.isEOL) source.popFront();
        token.stringv = source.sliceFrom(start);
    }
    else if(token.type == Type.blockComment)
    {
        while(!(source.isEOF || source.matches("*/"))) source.popFront();
        token.stringv = source.sliceFrom(start);
        if(!source.consumeIfMatch("*/"))
            source.error(`Expected "*/", got EOF.`);
    }
    else if(token.type == Type.codeComment)
    {
        auto level = 1;
        while(!source.isEOF && level > 0)
        {
            if(source.matches("+/"))
            {
                --level;
                level > 0 && source.consume("+/");
            }
            else if(source.consumeIfMatch("/+"))
                ++level;
            else
                source.popFront();
            assert(level >= 0);
        }

        if(level == 0)
        {
            token.stringv = source.sliceFrom(start);
            source.consume("+/");
        }
        else
            source.error(`Expected "+/", got EOF.`);
    }
    else assert(0);
}

void lexComment(ref Source source, ref Token token)
in
{
    assert(source.matchesAny("//", "/*", "/+"));
}
body
{
    switch(source.peek())
    {
        case '/':
            token.type = Type.lineComment;
            if(source.matches("///"))
                source.lexDdocComment(token);
            else if(source.matches("//"))
                source.lexRegularComment(token);
            else assert(0);
            break;
        case '*':
            token.type = Type.blockComment;
            if(source.matches("/**/"))
                source.lexRegularComment(token);
            else if(source.matches("/**"))
                source.lexDdocComment(token);
            else if(source.matches("/*"))
                source.lexRegularComment(token);
            else assert(0);
            break;
        case '+':
            token.type = Type.codeComment;
            if(source.matches("/++/"))
                source.lexRegularComment(token);
            else if(source.matches("/++"))
                source.lexDdocComment(token);
            else if(source.matches("/+"))
                source.lexRegularComment(token);
            else assert(0);
            break;
        default: assert(0, "invalid token identifier");
    }

}

bool isEOF(dchar ch)
{
    return ch == 0 || ch == 0x1A;
}

Token lex(ref Source source)
{
    auto start = source.mark();
    Token token;
    assert(token.type == Type._typeNotSet);
    token.location.start = token.location.end = source.location;
    scope(exit)
    {
        assert(token.type != Type._typeNotSet, text("Token type not set! Token: ", token));
        assert(token.raw == "", text("Token raw value already set! Token: ", token));
        token.location.end = source.location;
        token.raw = source.sliceFrom(start);
    }

    void single(Type type)
    {
        assert(token.type == Type._typeNotSet, "token.type set twice");
        assert(!source.isEOF, "cannot check for single token on an empty source");
        token.type = type;
        source.popFront();
    }

    bool singleIf(string match, Type type)
    {
        assert(token.type == Type._typeNotSet, "token.type set twice");
        assert(!source.isEOF, "cannot check for single token on an empty source");
        if(source.consumeIfMatch(match))
        {
            token.type = type;
            return true;
        }
        return false;
    }

    switch(source.front)
    {
        case 0:
        case 0x1A:  token.type = Type.EOF;          break;

        case ' ':   single(Type.space);             break;
        case '\t':  single(Type.tab);               break;
        case '\r':  single(Type.carriageReturn);    break;
        case '\n':  single(Type.lineFeed);          break;

        case '\'':  source.lexChar(token);         break;
        case '\\':  source.lexEscape(token);       break;

        case '0': .. case '9':
            source.lexNumber(token);               break;

        case 'q':
        case 'r':
        case 'x':
            if(source.peek().isValidIdentifier())
                goto Lidentifier;
            goto case; // it's a string
        case '`':
        case '"':
            source.lexString(token);
            break;

        case 'A': .. case 'Z':
        case 'a': .. case 'p': /*skip q and r*/
        case 's': .. case 'w': /*skip x*/
        case 'y': case 'z':
        case '_':
            assert(!source.matchesAny("x", "q", "r"), "string type, not an identifier");
Lidentifier:
            assert(source.front.isValidIdentifierStart());
            source.lexIdentifier(token);
            break;

        case '/':
            if(source.matchesAny("//", "/*", "/+"))
            {
                source.lexComment(token);
                assert(source.context == SourceContext.code, text("Entire comment not lexed!: ", token));
            }
            else
            {
                bool match =
                       singleIf("/=", Type.divAssign)
                    || singleIf( "/", Type.div);
                assert(match);
            }
            break;
        case '.':
            bool match =
                   singleIf("...",  Type.dot3)
                || singleIf( "..",  Type.range)
                || singleIf(  ".",  Type.dot);
            assert(match);
            break;
        case '&':
            bool match =
                   singleIf("&&",   Type.and2)
                || singleIf("&=",   Type.andAssign)
                || singleIf( "&",   Type.and);
            assert(match);
            break;
        case '|':
            bool match =
                   singleIf("||",   Type.or2)
                || singleIf("|=",   Type.orAssign)
                || singleIf( "|",   Type.or);
            assert(match);
            break;
        case '-':
            if(source.peek().isDigit())
                source.lexNumber(token);
            else
            {
                bool match =
                       singleIf("-=", Type.minusAssign)
                    || singleIf("--", Type.decrement)
                    || singleIf( "-", Type.minus);
                assert(match);
            }
            break;
        case '+':
            bool match =
                   singleIf("+=", Type.plusAssign)
                || singleIf("++", Type.increment)
                || singleIf( "+", Type.plus);
            assert(match);
            break;
        case '<':
            bool match =
                   singleIf("<=", Type.lessEquals)
                || singleIf("<<", Type.shiftLeft)
                || singleIf( "<", Type.less);
            assert(match);
            break;
        case '>':
            bool match =
                   singleIf(">=", Type.greaterEquals)
                || singleIf(">>", Type.shiftRight)
                || singleIf( ">", Type.greater);
            assert(match);
            break;
        case '=':
            bool match =
                   singleIf("==", Type.equals)
                || singleIf("=>", Type.goesTo)
                || singleIf( "=", Type.assign);
            assert(match);
            break;
        case '^':
            bool match =
                   singleIf("^^=", Type.powAssign)
                || singleIf( "^^", Type.pow)
                || singleIf( "^=", Type.xorAssign)
                || singleIf(  "^", Type.xor);
            break;

        case '(': single(Type.lparen);      break;
        case ')': single(Type.rparen);      break;
        case '[': single(Type.lbracket);    break;
        case ']': single(Type.rbracket);    break;
        case '{': single(Type.lcurly);      break;
        case '}': single(Type.rcurly);      break;

        case '!': single(Type.exclaim);     break;
        case '~': single(Type.tilde);       break;
        case '?': single(Type.question);    break;
        case ',': single(Type.comma);       break;
        case ';': single(Type.semicolon);   break;
        case ':': single(Type.colon);       break;
        case '$': single(Type.dollar);      break;
        case '@': single(Type.at);          break;
        case '*': single(Type.star);        break;
        case '%': single(Type.percent);     break;
        case '#': single(Type.hash);        break;
        default: assert(0, text("invalid character '", source.front, "'"));
    }
    return token;
}


// Some writer functions to better see what fails during testing
version(unittest)
{
    string[] _testName;
    int _testIndent = -1;

    @property string indent()
    {
        return rightJustify("", _testIndent*2);
    }

    @property string currentTest()
    {
        return _testName[_testIndent];
    }

    @property void currentTest(string text)
    {
        if(_testName.length <= _testIndent)
            _testName ~= text;
        else
            _testName[_testIndent] = text;
    }

    void testBegin(string text)
    {
        ++_testIndent;
        currentTest = text;
        debug(Unittest) writeln("\n", indent, "*** TEST: ", currentTest, " ***");
    }

    void testEnd()
    {
        debug(Unittest) writeln(indent, "*** DONE TEST: ", currentTest, " ***");
        --_testIndent;
    }

    void testHeader(string text)
    {
        debug(Unittest) writeln("\n", indent, ">>> ", text, ":");
    }

    void twriteln(Args ...)(Args args)
    {
        debug(Unittest) writeln(args);
    }
}

// Specialized functions to make testing and locating errors a bit simpler
// linux doesn't give me line numbers in stacktraces, so use the file/line workaround
version(unittest)
{
    Source source;
    Token token;

    void lexIt(string code, string file = __FILE__, size_t line = __LINE__)
    {
        twriteln(indent, "@", file, ":", line, ": `", code, "`");
        source = Source(code);
        token = lex(source);
    }

    void assume(string condition)(string file = __FILE__, size_t line = __LINE__)
    {
        mixin(`bool success = `~condition~`;`);
        if(!success)
        {
            // Cannot writeln(source) because of phobos bug http://d.puremagic.com/issues/show_bug.cgi?id=7476
            writeln(indent, "FAIL: `", condition, "`\n", indent,
                    "  TOKEN: ", token, "\n", indent,
                    "  SOURCE: ", source.location, " ", source.issues, " ", source.source);
            assume(success, file, line);
        }
    }
    void assume()(bool success, string file = __FILE__, size_t line = __LINE__)
    {
        if(!success)
            writeln(indent, "ASSERT@", file, ":", line);
        assert(success);
    }

    void assumeType(string code, Type type, string file = __FILE__, size_t line = __LINE__)
    {
        lexIt(code, file, line);
        assumeType(type, file, line);
    }

    void assumeType(Type type, string file = __FILE__, size_t line = __LINE__)
    {
        auto success = token.type == type;
        if(!success)
            twriteln(indent(), "!!! Expected ", type, " got ", token.type);
        assume(token.type == type, file, line);
    }

    void assumeIssueMessage(IssueType type, string msg, string file = __FILE__, size_t line = __LINE__)
    {
        if(source.issues.length == 0)
        {
            twriteln(indent, "!!! Expected `", msg, "`, but found no issues!");
            assume(false, file, line);
        }

        auto issue = source.issues[0];
        if(issue.type != type)
        {
            twriteln(indent, "!!! Expected ", type, ":`", msg, "`, but found ", issue);
            assume(false, file, line);
        }

        if(issue.message != msg)
            twriteln(indent, "!!! Expected `", msg, "`, but got `", issue.message, "`: ", issue);
        assume(issue.message == msg, file, line);
    }

    void assumeError(string code, string msg, string file = __FILE__, size_t line = __LINE__)
    {
        lexIt(code, file, line);
        assumeIssueMessage(IssueType.error, msg, file, line);
    }
}

unittest
{
    testBegin("TYPES");

    testHeader("EOF");
    assumeType("", Type.EOF);
    assumeType("\0", Type.EOF);
    assumeType("\x1A", Type.EOF);

    testHeader("Brackets");
    assumeType("(", Type.lparen);
    assumeType(")", Type.rparen);
    assumeType("[", Type.lbracket);
    assumeType("]", Type.rbracket);
    assumeType("{", Type.lcurly);
    assumeType("}", Type.rcurly);

    testHeader("Equal");
    assumeType("=", Type.assign);
    assumeType("==", Type.equals);

    testHeader("Whitespace");
    assumeType(" ", Type.space);
    assumeType("\t", Type.tab);
    assumeType("\r", Type.carriageReturn);
    assumeType("\n", Type.lineFeed);

    testHeader("Misc");
    assumeType(";", Type.semicolon);
    assumeType(":", Type.colon);
    assumeType(".", Type.dot);
    assumeType("..", Type.range);
    assumeType("...", Type.dot3);

    testHeader("Plus");
    assumeType("+", Type.plus);
    assumeType("+=", Type.plusAssign);
    assumeType("++", Type.increment);

    testHeader("Minus");
    assumeType("-", Type.minus);
    assumeType("-=", Type.minusAssign);
    assumeType("--", Type.decrement);

    testHeader("Equals");
    assumeType("=", Type.assign);
    assumeType("=>", Type.goesTo);
    assumeType("==", Type.equals);

    testHeader("Greater");
    assumeType(">", Type.greater);
    assumeType(">=", Type.greaterEquals);

    testHeader("Less");
    assumeType("<", Type.less);
    assumeType("<=", Type.lessEquals);

    testHeader("Hat");
    assumeType("^", Type.xor);
    assumeType("^^", Type.pow);
    assumeType("^^=", Type.powAssign);

    testHeader("Identifier");
    assumeType("a", Type.identifier);
    assumeType("_a", Type.identifier);
    assumeType("a_", Type.identifier);
    assumeType("qa", Type.identifier);
    assumeType("xa", Type.identifier);
    assumeType("ra", Type.identifier);

    testHeader("Keywords");
    assumeType("void", Type.void_);
    assumeType("bool", Type.bool_);
    assumeType("char", Type.char_);
    assumeType("wchar", Type.wchar_);
    assumeType("dchar", Type.dchar_);
    assumeType("auto", Type.auto_);

    assumeType("byte", Type.int8);
    assumeType("ubyte", Type.uint8);
    assumeType("short", Type.int16);
    assumeType("ushort", Type.uint16);
    assumeType("int", Type.int32);
    assumeType("uint", Type.uint32);
    assumeType("long", Type.int64);
    assumeType("ulong", Type.uint64);
    assumeType("float", Type.float32);
    assumeType("double", Type.float64);
    assumeType("real", Type.float80);

    assumeType("enum", Type.enum_);
    assumeType("struct", Type.struct_);
    assumeType("union", Type.union_);
    assumeType("interface", Type.interface_);
    assumeType("class", Type.class_);
    assumeType("alias", Type.alias_);
    assumeType("function", Type.function_);
    assumeType("delegate", Type.delegate_);
    assumeType("mixin", Type.mixin_);
    assumeType("override", Type.override_);

    assumeType("private", Type.private_);
    assumeType("protected", Type.protected_);
    assumeType("package", Type.package_);
    assumeType("public", Type.public_);
    assumeType("extern", Type.extern_);

    assumeType("static", Type.static_);
    assumeType("final", Type.final_);
    assumeType("const", Type.const_);
    assumeType("immutable", Type.immutable_);
    assumeType("abstract", Type.abstract_);
    assumeType("in", Type.in_);
    assumeType("out", Type.out_);
    assumeType("ref", Type.ref_);
    assumeType("inout", Type.inout_);
    assumeType("lazy", Type.lazy_);

    assumeType("debug", Type.debug_);
    assumeType("version", Type.version_);
    assumeType("deprecated", Type.deprecated_);

    assumeType("module", Type.module_);
    assumeType("import", Type.import_);

    assumeType("if", Type.if_);
    assumeType("else", Type.else_);
    assumeType("do", Type.do_);
    assumeType("while", Type.while_);
    assumeType("for", Type.for_);
    assumeType("switch", Type.switch_);
    assumeType("case", Type.case_);
    assumeType("default", Type.default_);
    assumeType("break", Type.break_);
    assumeType("continue_", Type.continue_);
    assumeType("with", Type.with_);
    assumeType("synchronized", Type.synchronized_);
    assumeType("return", Type.return_);
    assumeType("goto", Type.goto_);
    assumeType("throw", Type.throw_);
    assumeType("try", Type.try_);
    assumeType("catch", Type.catch_);
    assumeType("finally", Type.finally_);
    assumeType("foreach", Type.foreach_);
    assumeType("foreach_reverse", Type.foreach_reverse_);
    assumeType("scope", Type.scope_); // scope(exit), failure / success is it's own types to not use keywords!!
    // TODO: add scope(otherstuff) here

    assumeType("body", Type.body_);
    assumeType("invariant", Type.invariant_);
    assumeType("macro", Type.macro_);

    assumeType("pure", Type.pure_);
    assumeType("nothrow", Type.nothrow_);
    assumeType("shared", Type.shared_);
    assumeType("__gshared", Type.gshared);
    assumeType("@", Type.at);
    assumeType("^^", Type.pow);
    assumeType("^^=", Type.powAssign);

    assumeType("unittest", Type.unittest_);

    testHeader("Div");
    assumeType("/", Type.div);
    assumeType("/=", Type.divAssign);

    assumeType("__FILE__", Type.FILE);
    assumeType("__LINE__", Type.LINE);
    assumeType("__TIME__", Type.TIME);
    assumeType("__TIMESTAMP__", Type.TIMESTAMP);
    assumeType("__DATE__", Type.DATE);
    assumeType("__VENDOR__", Type.VENDOR);
    assumeType("__VERSION__", Type.VERSION);

    testEnd();
}

unittest
{
    testBegin("CHAR ESCAPE");
        lexIt(`'\''`);
        writeln(token.char8v);
        assumeType(`'\''`, Type.char8v);
        assume!q{token.char8v == '\''}();
        assumeType(`'\"'`, Type.char8v);
        assume!q{token.char8v == '\"'}();
        assumeType(`'\?'`, Type.char8v);
        assume!q{token.char8v == '\?'}();
        assumeType(`'\\'`, Type.char8v);
        assume!q{token.char8v == '\\'}();
        assumeType(`'\a'`, Type.char8v);
        assume!q{token.char8v == '\a'}();
        assumeType(`'\b'`, Type.char8v);
        assume!q{token.char8v == '\b'}();
        assumeType(`'\f'`, Type.char8v);
        assume!q{token.char8v == '\f'}();
        assumeType(`'\n'`, Type.char8v);
        assume!q{token.char8v == '\n'}();
        assumeType(`'\r'`, Type.char8v);
        assume!q{token.char8v == '\r'}();
        assumeType(`'\t'`, Type.char8v);
        assume!q{token.char8v == '\t'}();
        assumeType(`'\v'`, Type.char8v);
        assume!q{token.char8v == '\v'}();
        assumeType(`'\0'`, Type.char8v);
        assume!q{token.char8v == '\0'}();

        assumeType(`'\x00'`, Type.char8v);
        assume!q{token.char8v == '\x00'}();
        assumeType(`'\x1A'`, Type.char8v);
        assume!q{token.char8v == '\x1A'}();
    testEnd();
}

unittest
{
    testBegin("COMMENT");

    // eof valid end
    assumeType("//", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//`}();
    assume!q{token.stringv == ``}();

    assumeType("//\n", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//`}();
    assume!q{token.stringv == ``}();

    assumeType("//a", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//a`}();
    assume!q{token.stringv == `a`}();

    assumeType("//a\n", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//a`}();
    assume!q{token.stringv == `a`}();

    assumeType("//comment here", Type.lineComment);
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//comment here`}();
    assume!q{token.stringv == `comment here`}();

    assumeType("//+", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//+`}();
    assume!q{token.stringv == `+`}();

    assumeType("//*", Type.lineComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `//*`}();
    assume!q{token.stringv == `*`}();

    // Should be error as */ is missing
    assumeError("/*", `Expected "*/", got EOF.`);
    assumeType("/*", Type.blockComment);
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == "/*"}();
    assume!q{token.stringv == ``}();

    // Should be error as +/ is missing
    assumeError("/+", `Expected "+/", got EOF.`);
    assumeType("/+", Type.codeComment);
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == "/+"}();
    assume!q{token.stringv == ``}();

    assumeType("/*a*/", Type.blockComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `/*a*/`}();
    assume!q{token.stringv == "a"}();

    assumeType("/**/", Type.blockComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `/**/`}();
    assume!q{token.stringv == ""}();

    assumeType("/*some comment*/", Type.blockComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `/*some comment*/`}();
    assume!q{token.stringv == `some comment`}();

    assumeType("/++/", Type.codeComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `/++/`}();
    assume!q{token.stringv == ``}();

    assumeType("/+/++/+/", Type.codeComment);
    assume!q{source.issues.length == 0}();
    assume!q{source.context == SourceContext.code}();
    assume!q{token.raw == `/+/++/+/`}();
    assume!q{token.stringv == `/++/`}();

    testEnd();
}

unittest
{
    /+
    testBegin("HEX ESCAPE");
        testHeader("CORRECT");
        assumeType(`\xFFFF`, Type.uint16v);
        assume!q{token.uint16v == 0xFF_FF}();
        assumeType(`\xFF_FF`, Type.uint16v);
        assume!q{token.uint16v == 0xFF_FF}();

        assumeType(`\xFFFFFFFF`, Type.uint16v);
        assume!q{token.uint16v == 0xFF_FF}();
        assumeType(`\xFF_FF_FF_FF`, Type.uint16v);
        assume!q{token.uint16v == 0xFF_FF}();

        assumeType(`\uFFFFFFFF`, Type.uint32v);
        assume!q{token.uint32v == 0xFF_FF_FF_FF}();
        assumeType(`\uFF_FF_FF_FF`, Type.uint32v);
        assume!q{token.uint32v == 0xFF_FF_FF_FF}();

        assumeType(`\uFFFFFFFFFFFFFFFF`, Type.uint32v);
        assume!q{token.uint32v == 0xFF_FF_FF_FF}();
        assumeType(`\uFF_FF_FF_FF_FF_FF_FF_FF`, Type.uint32v);
        assume!q{token.uint32v == 0xFF_FF_FF_FF}();

        assumeType(`\UFFFFFFFFFFFFFFFF`, Type.uint64v);
        assume!q{token.uint64v == 0xFF_FF_FF_FF_FF_FF_FF_FF}();
        assumeType(`\UFF_FF_FF_FF_FF_FF_FF_FF`, Type.uint64v);
        assume!q{token.uint64v == 0xFF_FF_FF_FF_FF_FF_FF_FF}();

        testHeader("CHOOSES SMALLEST TYPE");
        twriteln("!!! TODO: Add these tests");

        testHeader("ERRORS");
        assumeType(`\xFF`, Type.invalid);
        assumeError(`\xFF`, "\\x hex escape needs 4 hex characters, got 2.");
        assume!q{token.type == Type.invalid}();
        assumeError(`\xJ`, "Expected hex character, got 'J'.");
        assume!q{token.type == Type.invalid}();
        assumeError(`\xFF_FJ`, "Expected hex character, got 'J'.");
        assume!q{token.type == Type.invalid}();
        assumeError(`\xJF_FF`, "Expected hex character, got 'J'.");
        assume!q{token.type == Type.invalid}();
        assumeError(`\uFF_FF_FF`, "\\u hex escape needs 8 hex characters, got 6.");
        assume!q{token.type == Type.invalid}();
        assumeError(`\UFF_FF_FF_FF_FF_FF_FF`, "\\U hex escape needs 16 hex characters, got 14.");
        assume!q{token.type == Type.invalid}();
    testEnd();
    +/
}

unittest
{
    testBegin("DDOC");
    twriteln("!!! TODO: ADD TESTS");
    version(none)
    {
        assumeType("///", Type.lineComment);
        assume!q{source.context == SourceContext.ddoc}();

        assumeType("/**", Type.blockComment);
        assume!q{source.context == SourceContext.ddoc}();

        assumeType("/++", Type.codeComment);
        assume!q{source.context == SourceContext.ddoc}();
    }
    testEnd();
}

unittest
{
    // Same rules as hex escape, so we wont do much testing here
    testBegin("HEX LITERAL");
        assumeType("0xFF", Type.uint8v);
    testEnd();
}

unittest
{
    testBegin("INTEGER");
        assumeType(to!string(byte.min), Type.int32v);
        assume!q{token.int8v == byte.min}();
        assumeType(to!string(short.min), Type.int32v);
        assume!q{token.int16v == short.min}();
        assumeType(to!string(int.min), Type.int32v);
        assume!q{token.int32v == int.min}();

        /*
        assumeType(to!string(long.min), Type.int64v);
        assume!q{token.int64v == long.min}();

        assumeType(to!string(byte.max), Type.int32v);
        assume!q{token.int8v == byte.max}();
        assumeType(to!string(short.max), Type.int32v);
        assume!q{token.int16v == short.max}();
        assumeType(to!string(int.max), Type.int32v);
        assume!q{token.int32v == int.max}();
        assumeType(to!string(long.max), Type.int64v);
        assume!q{token.int64v == long.max}();

        assumeType(`0`, Type.int32v);
        assume!q{token.boolv == false}();
        assumeType(`1`, Type.int32v);
        assume!q{token.uint8v == true}();
        assumeType(`2`, Type.int32v);
        assume!q{token.int8v == 2}();
        assumeType(`-1`, Type.int32v);
        assume!q{token.int8v == -1}();

        assumeType(to!string(byte.max+1), Type.int32v);
        assume!q{token.uint8v == byte.max+1}();
        // signed.min - 1 == larger signed
        assumeType(to!string(byte.min-1), Type.int32v);
        assume!q{token.int16v == byte.min-1}();
        // unsigned.max + 1 == larger signed
        assumeType(to!string(ubyte.max+1), Type.int32v);
        assume!q{token.int16v == ubyte.max+1}();
        */
    testEnd();
}

unittest
{
    // FIXME: should use smallest type
    testBegin("BINARY LITERAL");
        assumeType("0b00", Type.int32v);
        assume!q{token.int32v == 0b00}();
        assumeType("0b10", Type.int32v);
        assume!q{token.int32v == 0b10}();
        assumeType("0b01", Type.int32v);
        assume!q{token.int32v == 0b01}();
        assumeType("0b1_1", Type.int32v);
        assume!q{token.int32v == 0b11}();
    testEnd();
}
unittest
{
    testBegin("FLOAT");
    testEnd();
}

unittest
{
    testBegin("STRING");
        assumeType(`"'"`, Type.stringv);
        assume!q{token.stringv == "'"}();
        assumeType(`""`, Type.stringv);
        assume!q{token.raw == `""`}();
        assume!q{token.stringv == ""}();
        assumeType(`""c`, Type.stringv);
        assume!q{token.raw == `""c`}();
        assume!q{token.stringv == ""c}();
        assumeType(`""w`, Type.wstringv);
        assume!q{token.raw == `""w`}();
        assume!q{token.stringv == ""c}(); // stored as utf-8 internally
        assumeType(`""d`, Type.dstringv);
        assume!q{token.raw == `""d`}();
        assume!q{token.stringv == ""c}(); // stored as utf-8 internally

        assumeType(`"\x1A"`, Type.stringv);
        assume!q{token.stringv == "\x1A"}();

        assumeType(`"\""`, Type.stringv);
        assume!q{token.stringv == `"`}();
        assumeType(`" \""`, Type.stringv);
        assume!q{token.stringv == ` "`}();
        assumeType(`" \" "`, Type.stringv);
        assume!q{token.stringv == ` " `}();
        assumeType(`"'"`, Type.stringv);
        assume!q{token.stringv == "'"}();
        assumeType(`" \" '"`, Type.stringv);
        assume!q{token.raw == `" \" '"`}();
        assume!q{token.stringv == ` " '`}();
    testEnd();

}

unittest
{
    testBegin("TOKEN STRING");
        assumeType(`q{}`, Type.stringv);
        assume!q{token.raw == `q{}`}();
        assume!q{token.stringv == ``}();
    testEnd();
}

unittest
{
    testBegin("DELIMITED STRING");
        // single symbol delimited string
        assumeType(`q"/foo/"`, Type.stringv);
        assume!q{token.stringv == q"/foo/"}();
        // nested delimited string
        assumeType(`q"[f[o[o]]]"`, Type.stringv);
        assume!q{token.stringv == q"[f[o[o]]]"}();
        // ident delimited string
        assumeType(
`q"EOF
somestring
EOF"`, Type.stringv);
        assume!q{token.stringv ==
q"EOF
somestring
EOF"}();
        testEnd();
}

unittest
{
    testBegin("WYSIWYG");
        assumeType("``", Type.stringv);
        assumeType("``c", Type.stringv);
        assumeType("``w", Type.wstringv);
        assumeType("``d", Type.dstringv);

        assumeType("`''`", Type.stringv);
        //assumeType("`'\\''`", Type.stringv);
        lexIt("`\\`");
        lexIt("`\\`");
        lexIt("`'\\''`");
        writeln(token.stringv);
    testEnd();
}

/+
void main()
{
    import std.datetime;
    auto source = Source(import("lexer.d"));
    Token token;
    write("1: "); // line 1
    auto sw = StopWatch(AutoStart.yes);
    do
    {
        token = lex(source);
        version(none)
        {
        write(token.raw);
        if(token.type == Type.lineFeed)
            write(token.location.end.line, ": ");
        }
    } while(token.type != Type.EOF);

    writeln("Time (msecs): ", sw.peek().msecs);
    writeln("Issues:");
    foreach(issue; source.issues)
        writeln(issue);
}
+/
