#pragma once
#include <stdio.h>
#include <stdint.h>
#include <crtdbg.h>
#define _CRTDBG_MAP_ALLOC
#define new new( _NORMAL_BLOCK, __FILE__, __LINE__)

#include <string>
#include <vector>

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#define assert(Expr) if(!(Expr)) { *(int*)0 = 0; exit(-1); }

#define NotImplemented() assert(0)

struct LeakChecker {
    char* Name = 0;
    _CrtMemState _ms;
    LeakChecker(char* Name) : Name(Name) {
        _CrtMemCheckpoint(&_ms);
    }
    ~LeakChecker() {
        if (Name)
            printf("LeakChecker \"%s\"\n", Name);
        _CrtMemDumpAllObjectsSince(&_ms);
    }
};
#define STRINGIZE_(x) #x
#define STRINGIZE(x) STRINGIZE_(x)
#define CONCAT(a, b) a ## b
#define CONCAT2(a, b) CONCAT(a, b)
#define LEAK_CHECKER(name) LeakChecker CONCAT2(LeakChecker_, __LINE__)(name)

using s8  = int8_t;
using s16 = int16_t;
using s32 = int32_t;
using b32 = s32;
using s64 = int64_t;
using u8  = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using f32 = float;
using f64 = double;

template<class type> type Min(type A, type B) { return A > B ? B : A; }
template<class type> type Max(type A, type B) { return A > B ? A : B; }

template<class iterator>
struct range {
    iterator Begin, End;
    range(iterator Begin, iterator End) : Begin(Begin), End(End) {}
    inline iterator begin() { return Begin; }
    inline iterator end() { return End; }
};
template<class container>
inline auto Reverse(container& Container) {
    return range(std::rbegin(Container), std::rend(Container));
}
struct string_span {
    char* Begin = 0;
    char* End = 0;
    inline string_span() {}
    inline string_span(char* Begin, char* End) : Begin(Begin), End(End) {}
    inline string_span(char* Strz) : Begin(Strz), End(Strz + strlen(Strz)) {}
    inline string_span(char* String, size_t Count) : Begin(String), End(String + Count) {}
    inline string_span(std::string& String) : Begin(String.data()), End(String.data() + String.size()) {}
    inline size_t Count() const { return End - Begin; }
    inline char* begin() { return Begin; }
    inline char* end() { return End; }
    inline char& operator[](u32 I) { return Begin[I]; }
    inline char operator[](u32 I) const { return Begin[I]; }
};

struct string_view {
    const char* Begin = 0;
    const char* End = 0;
    inline string_view() {}
    inline string_view(const char* Begin, const char* End) : Begin(Begin), End(End) {}
    inline string_view(const char* Strz) : Begin(Strz), End(Strz + strlen(Strz)) {}
    inline string_view(const char* String, size_t Count) : Begin(String), End(String + Count) {}
    inline string_view(const std::string& String) : Begin(String.data()), End(String.data() + String.size()) {}
    inline string_view(string_span Span) : Begin(Span.Begin), End(Span.End) {}

    inline size_t Count() const { return End - Begin; }
    inline const char* begin() const { return Begin; }
    inline const char* end() const { return End; }
    inline char operator[](u32 I) const { return Begin[I]; }
};

struct location {
    string_view TokenView;
    string_view LineView[3];
    string_view FileView;
    u32 Line = 0;
    u32 Column = 0;
};

inline bool StringsAreEqual(const char* A, const char* B, size_t Count) {
    for (int I = 0; I < Count; ++I) {
        if (*A++ != *B++)
            return false;
    }
    return true;
}
inline bool StringsAreEqualSafe(const char* A, const char* B, size_t Count) {
    for (int I = 0; I < Count; ++I) {
        if (*A ^ *B) {
            return false;
        }
        if (*A++ != *B++)
            return false;
    }
    return true;
}
inline bool operator==(string_view A, string_view B) {
    if (A.Count() != B.Count())
        return false;
    return StringsAreEqual(A.Begin, B.Begin, A.Count());
}
inline bool operator==(string_view A, char* B) {
    string_view BView;
    BView.Begin = B;
    BView.End = B;
    while (*BView.End) ++BView.End;
    return A == BView;
}
inline bool operator==(string_view A, std::string& B) {
    if (A.Count() != B.length())
        return false;
    return StringsAreEqual(A.Begin, B.data(), A.Count());
}
inline bool operator==(char* A, string_view B) { return B == A; }
inline bool operator==(std::string& A, string_view B) { return B == A; }
inline bool operator!=(string_view A, string_view B) { return !(A == B); }
inline bool operator!=(string_view A, char* B) { return !(A == B); }
inline bool operator!=(char* A, string_view B) { return !(A == B); }
inline bool operator!=(string_view A, std::string& B) { return !(A == B); }
inline bool operator!=(std::string& A, string_view B) { return !(A == B); }

namespace std {

template <>
struct hash<::string_view> {
    size_t operator()(::string_view String) const {
        size_t Result = 0;
        for (; String.Begin != String.End; ++String.Begin) {
            Result ^= hash<char>()(*String.Begin);
        }
        return Result;
    }
};

}

inline std::string ToString(string_view String) {
    std::string Result;
    Result.resize(String.Count());
    memcpy(Result.data(), String.Begin, String.Count());
    return Result;
}
inline void Set(std::string& A, string_view B) {
    A.assign(B.Begin, B.End);
}
template<class UInt>
u32 HighestOneBitPosition(UInt A) {
    static_assert(std::is_unsigned_v<UInt>);
    u32 Bits = 0;
    while (A != 0) {
        A >>= 1;
        ++Bits;
    }
    return Bits;
}
bool MulIsSafe(u32 A, u32 B) {
    return HighestOneBitPosition(A) + HighestOneBitPosition(B) <= 32;
}
bool MulIsSafe(u64 A, u64 B) {
    return HighestOneBitPosition(A) + HighestOneBitPosition(B) <= 64;
}
const string_view DefaultDigitTable = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
template<class UInt>
u32 ToString(char* Out, UInt Val, u32 Radix = 10, bool ZeroExtent = false, string_view DigitTable = DefaultDigitTable) {
    static_assert(std::is_unsigned_v<UInt>);
    assert(Radix > 1);
    assert(Radix <= DigitTable.Count());
    u32 CharIdx = 0;
    UInt Div = Radix;
    while (MulIsSafe(Div, (UInt)Radix))
        Div *= Radix;
    if (ZeroExtent) {
        while (Div > 0) {
            Out[CharIdx] = DigitTable[(Val / Div) % Radix];
            CharIdx += 1;
            Div /= Radix;
        }
    }
    else {
        bool Put = false;
        while (Div > 0) {
            char Result = DigitTable[(Val / Div) % Radix];
            if ((!Put) && (Result != DigitTable[0]))
                Put = true;
            if (Put) {
                Out[CharIdx] = Result;
                CharIdx += 1;
            }
            Div /= Radix;
        }
        if (CharIdx == 0)
            Out[CharIdx++] = DigitTable[0];
    }
    return CharIdx;
}
struct string_builder {
    struct block {
        static constexpr size_t Capacity = 1024;
        char Data[Capacity];
        block* Next = {};
    };
    block Block = {};
    block* LastBlock = &Block;
    char* Cursor = Block.Data;
    void AppendBlock() {
        LastBlock->Next = new block;
        LastBlock = LastBlock->Next;
        Cursor = LastBlock->Data;
    }
    size_t GetRemainingCapacity() {
        return LastBlock->Data + block::Capacity - Cursor;
    }
    void Append(string_view String) {
        size_t RemainingCapacity = GetRemainingCapacity();
        if (RemainingCapacity == 0) {
            AppendBlock();
            RemainingCapacity = block::Capacity;
        }
        size_t CharsToWrite = String.Count();
        while (CharsToWrite > RemainingCapacity) {
            size_t CharsWritten = RemainingCapacity;
            memcpy(Cursor, String.Begin, CharsWritten);
            AppendBlock();
            RemainingCapacity = block::Capacity;
            String.Begin += CharsWritten;
            CharsToWrite -= CharsWritten;
        }
        if (CharsToWrite) {
            memcpy(Cursor, String.Begin, CharsToWrite);
            Cursor += CharsToWrite;
        }
    }
    void Append(const char* String) {
        Append(string_view(String));
    }
    void Append(char Value) {
        if (GetRemainingCapacity() == 0) {
            AppendBlock();
        }
        *Cursor++ = Value;
    }
    template<class UInt, class = std::enable_if_t<std::is_unsigned_v<UInt>>>
    void Append(UInt Value, u32 Radix = 10, bool ZeroExtent = false, string_view DigitTable = DefaultDigitTable) {
        char Buffer[sizeof(UInt) * 8];
        auto Count = ToString(Buffer, Value, Radix, ZeroExtent, DigitTable);
        Append(string_view(Buffer, Count));
    }
    void Append(const void* Value, int Radix = 16, bool ZeroExtent = true, string_view DigitTable = DefaultDigitTable) {
        Append((u64)Value, Radix, ZeroExtent, DigitTable);
    }
    template<class type>
    void AppendBinary(const type& Value) {
        Append(string_view((char*)&Value, sizeof(type)));
    }
#if 0
    template<class type0>
    void AppendFormat(char* Format, const type0& Val0) {
        string_view Fmt = Format;
        for (char* C = Fmt.Begin; C < Fmt.End; ++C)
            if (*C == '%')
                Append(Val0);
            else
                Append(*C);
    }
    template<class type0, class type1>
    void AppendFormat(char* Format, const type0& Val0, const type1& Val1) {
        string_view Fmt = Format;
        u32 Position = 0;
        for (char* C = Fmt.Begin; C < Fmt.End; ++C)
            if (*C == '%') {
                switch (Position++) {
                    case 0: Append(Val0); break;
                    case 1: Append(Val1); break;
                    default: assert(0);
                }
            }
            else
                Append(*C);
    }
    template<class type0, class type1, class type2>
    void AppendFormat(char* Format, const type0& Val0, const type1& Val1, const type2& Val2) {
        string_view Fmt = Format;
        u32 Position = 0;
        for (char* C = Fmt.Begin; C < Fmt.End; ++C)
            if (*C == '%') {
                switch (Position++) {
                    case 0: Append(Val0); break;
                    case 1: Append(Val1); break;
                    case 2: Append(Val2); break;
                    default: assert(0);
                }
            }
            else
                Append(*C);
    }
    template<class type0, class type1, class type2, class type3>
    void AppendFormat(char* Format, const type0& Val0, const type1& Val1, const type2& Val2, const type3& Val3) {
        string_view Fmt = Format;
        u32 Position = 0;
        for (char* C = Fmt.Begin; C < Fmt.End; ++C)
            if (*C == '%') {
                switch (Position++) {
                    case 0: Append(Val0); break;
                    case 1: Append(Val1); break;
                    case 2: Append(Val2); break;
                    case 3: Append(Val3); break;
                    default: assert(0);
                }
            }
            else
                Append(*C);
    }
#endif
    std::string GetString() const {
        size_t Count = 0;
        auto CurrentBlock = &Block;
        while (true) {
            if (CurrentBlock->Next) {
                Count += block::Capacity;
                CurrentBlock = CurrentBlock->Next;
                continue;
            }
            else {
                Count += Cursor - CurrentBlock->Data;
                break;
            }
        }
        std::string Result(Count, '\0');
        char* ResultCursor = Result.data();
        CurrentBlock = &Block;
        while (true) {
            if (CurrentBlock->Next) {
                memcpy(ResultCursor, CurrentBlock->Data, block::Capacity);
                ResultCursor += block::Capacity;
                CurrentBlock = CurrentBlock->Next;
                continue;
            }
            else {
                memcpy(ResultCursor, CurrentBlock->Data, Cursor - CurrentBlock->Data);
                break;
            }
        }
        return Result;
    }
};

void puts(string_view String) {
    HANDLE Handle = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteConsoleA(Handle, String.Begin, (DWORD)String.Count(), 0, 0);
    char NewLine = '\n';
    WriteConsoleA(Handle, &NewLine, 1, 0, 0);
}

template<class type>
struct result {
    type Result;
    bool Failed;
};

u32 ToU32_Unchecked(string_view String) {
    u32 Result = 0;
    u32 Mult = 1;
    for (const char* C = String.End - 1; C >= String.Begin; --C) {
        Result += u32(*C - '0') * Mult;
        Mult *= 10;
    }
    return Result;
}

u64 ToU64_Unchecked(string_view String) {
    u64 Result = 0;
    u64 Mult = 1;
    for (const char* C = String.End - 1; C >= String.Begin; --C) {
        Result += u64(*C - '0') * Mult;
        Mult *= 10;
    }
    return Result;
}


template<class callable>
struct defer {
    callable C;
    defer(callable&& C) : C(C) {}
    ~defer() { C(); }
};
enum class data_type : u8 {
    Null,
    UInt,
    SInt,
    Float,
    Char,
    String,
    Bool,
};
char* ToString(data_type Type) {
    switch (Type) {
        case data_type::UInt:  return "uint";
        case data_type::SInt:  return "sint";
        case data_type::Float: return "float";
        case data_type::Char: return "char";
        case data_type::String: return "string";
        case data_type::Bool: return "bool";
        default:
            return "(unknown)";
    }
}
struct message {
    static constexpr u32 BufferSize = 256;
    char Buffer[BufferSize];
    string_span String = {Buffer, Buffer};
    location Location;
    message() = default;
    message(const message& M) {
        memcpy(Buffer, M.String.Begin, M.String.Count());
        String = {Buffer, M.String.Count()};
        Location = M.Location;
    }
    struct column {
        u32 Size = 0;
        column() = default;
        column(u32 Size) : Size(Size) {}
    };
    column Column;
};
inline u32 Append(message& Message, message::column Column) {
    Message.Column = Column;
    return 0;
}
inline u32 Append(message& Message, string_view Val) {
    if (!Val.Count())
        return 0;
    assert(Message.String.Count() + Val.Count() <= message::BufferSize);
    memcpy(Message.String.End, Val.Begin, Val.Count());
    Message.String.End += Val.Count();
    if (Message.Column.Size) {
        auto Column = Message.Column;
        Message.Column = {};
        if (Val.Count() < Column.Size) {
            Append(Message, std::string(Column.Size - Val.Count(), ' '));
            return Column.Size;
        }
    }
    return Val.Count();
}
inline u32 Append(message& Message, const char* Val) {
    return Append(Message, string_view(Val));
}
inline u32 Append(message& Message, char* Val) {
    return Append(Message, (const char*)Val);
}
inline u32 Append(message& Message, const std::string& Val) {
    return Append(Message, string_view(Val));
}
inline u32 Append(message& Message, char Char) {
    return Append(Message, string_view(&Char, 1));
}
inline u32 Append(message& Message, u32 Val) {
    char Buf[10];
    ultoa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline u32 Append(message& Message, u64 Val) {
    char Buf[20];
    _ui64toa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline u32 Append(message& Message, s32 Val) {
    char Buf[11];
    itoa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline u32 Append(message& Message, s64 Val) {
    char Buf[21];
    _i64toa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline u32 Append(message& Message, location Location) {
    char LineStr[16], ColumnStr[16];
    _itoa(Location.Line, LineStr, 10);
    _itoa(Location.Column, ColumnStr, 10);
    auto Column = Message.Column;
    Message.Column = {};
    u32 Result = 0;
    Result += Append(Message, Location.FileView);
    Result += Append(Message, ':');
    Result += Append(Message, LineStr);
    Result += Append(Message, ':');
    Result += Append(Message, ColumnStr);
    if (Result < Column.Size) {
        Append(Message, std::string(Column.Size - Result, ' '));
        return Column.Size;
    }
    return Result;
}

template<class type, class ...types>
inline u32 Append(message& Message, const type& Type, const types&... Types) {
    return Append(Message, Type) + Append(Message, Types...);
}
struct lexer;
struct parser;
struct compiler {
    lexer* Lexer = 0;
    parser* Parser = 0;
    string_view SourceFileName;
    string_view Source;
    char CurrentDir[512];
    char CompilerDir[512];
    char TempDir[512];
    char BinDir[512];
    int ExitCode;
    void Compile(string_view CompilerPath);
    void Free();
    void (*ReportMessageFn)(compiler*, const message&) = 0;
    void ReportMessage(const message& Msg) {
        ReportMessageFn(this, Msg);
    }
    // NOTE: Add messages only using 'ReportMessage'!
    std::vector<message> Messages;

    std::vector<void*> FilesContents;
    string_view LoadFileContents(FILE* File) {
        fseek(File, 0, SEEK_END);
        auto FileSize = ftell(File);
        fseek(File, 0, SEEK_SET);

        auto FileContents = new char[FileSize];
        FilesContents.push_back(FileContents);
        fread(FileContents, FileSize, 1, File);
        return {FileContents, (size_t)FileSize};
    }

    std::vector<std::string> CompilingFilePaths;
};
compiler* GlobalCompiler = 0;
inline void ReportMessage() {
    GlobalCompiler->ReportMessage({});
}
template<class ...types>
inline void ReportMessage(types... Types) {
    message Message;
    Append(Message, Types...);
    GlobalCompiler->ReportMessage(Message);
}
#if 1
template<class ...types>
inline void ReportError_(string_view File, u32 Line, location Location, types... Types) {
    message Message;
    Append(Message, File);
    Append(Message, ':');
    Append(Message, Line);
    Append(Message, ":\n");
    Append(Message, Location);
    Append(Message, ": Error: ");
    Append(Message, Types...);
    Message.Location = Location;
    GlobalCompiler->ReportMessage(Message);
}
#define ReportError(Loc, ...) ReportError_(__FILE__, __LINE__, Loc, __VA_ARGS__)
#else
template<class ...types>
inline void ReportError(location Location, types... Types) {
    message Message;
    Append(Message.String, CompiledFile);
    Append(Message.String, Location);
    Append(Message.String, ": ");
    Append(Message.String, Types...);
    Message.Location = Location;
    Messages.emplace_back(std::move(Message));
}
#endif
bool IsPowerOf2(u32 Value) {
    bool Found = false;
    for (u32 i = 0; i < 32; ++i) {
        if (Value & (1 << i)) {
            if (Found)
                return false;
            Found = true;
        }
    }
    return true;
}
bool IsRelativePath(string_view Path) {
    if (Path.Count()) {
        if (Path.Count() >= 2) {
            return Path[1] != ':';
        }
        else {
            return true;
        }
    }
    else {
        return true;
    }
}
#if 0
struct s80 {
    s64 High = 0;
    u8 Low = 0;
    s80() = default;
    s80(u16 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(u32 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(u64 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(s16 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(s32 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(s64 Val) : High(Val / 0x100), Low(Val % 0x100) {}
    s80(s64 H, u8 L) : High(H), Low(L) {}
    u32 ToU32() {
        return High * 0x100 + Low;
    }
};
bool operator==(s80 a, s80 b) {
    return a.Low == b.Low && a.High == b.High;
}
bool operator!=(s80 a, s80 b) {
    return !(a == b);
}
bool operator<(s80 a, s80 b) {
    if (a.High < b.High)
        return true;
    else if (a.High > b.High)
        return true;
    return a.Low < b.Low;
}
bool operator>(s80 a, s80 b) {
    if (a.High > b.High)
        return true;
    else if (a.High < b.High)
        return true;
    return a.Low > b.Low;
}
bool operator<=(s80 a, s80 b) {
    return !(a > b);
}
bool operator>=(s80 a, s80 b) {
    return !(a < b);
}
s80 operator+(s80 a, s80 b) {
    a.Low += b.Low;
    a.High += b.High;
    return a;
}
s80 operator-(s80 a, s80 b) {
    a.Low -= b.Low;
    a.High -= b.High;
    return a;
}
s80 operator*(s80 a, s80 b) {
    s64 Mult = b.High * 0x100 + b.Low;
    s64 Low = a.Low * Mult;
    a.Low = (u8)Low;
    a.High *= Mult;
    a.High += Low / 0x100;
    return a;
}
s80 operator/(s80 a, s80 b) {
    // HACK:
    s64 A = a.High * 0x100 + a.Low;
    s64 B = b.High * 0x100 + b.Low;
    A /= B;
    return {A / 0x100, (u8)(A & 0xFF)};
}
s80 operator%(s80 a, s80 b) {
    // HACK:
    s64 A = a.High * 0x100 + a.Low;
    s64 B = b.High * 0x100 + b.Low;
    A %= B;
    return {A / 0x100, (u8)(A & 0xFF)};
}

void Append(std::string& Str, s80 Val) {
    if (Val.High)
        Str.append(std::to_string(Val.High));
    Str.append(std::to_string(Val.Low));
}
#endif
