#pragma once
#pragma warning(disable:4996)
#include <stdio.h>
#include <stdint.h>
#include <crtdbg.h>
#define _CRTDBG_MAP_ALLOC
#define new new( _NORMAL_BLOCK, __FILE__, __LINE__)

#include <string>
#include <vector>
#include <utility>

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#define COUNT_OF(x) (sizeof(x) / sizeof(*x))

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

#ifdef BUILD_DEBUG
#define assert(Expr) if(!(Expr)) { *(int*)0 = 0; exit(-1); }
#else
#define assert(Expr) if(!(Expr)) { puts("Assertion failed: " #Expr); puts("File: " __FILE__); puts("Line: " STRINGIZE(__LINE__)); exit(-1); }
#endif
#define NotImplemented() assert(0)

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
using wchar = wchar_t;

template<class type>
auto CopyBuffer(type* Dst, const type* Src, size_t Count) {
    return memcpy(Dst, Src, Count * sizeof(type));
}
auto StrCpy(char* Dst, const char* Src) {
    return strcpy(Dst, Src);
}
auto StrCpy(wchar* Dst, const wchar* Src) {
    return wcscpy(Dst, Src);
}
auto StrLen(const char* Src) {
    return strlen(Src);
}
auto StrLen(const wchar* Src) {
    return wcslen(Src);
}
template<class type> type Min(type A, type B) { return A > B ? B : A; }
template<class type> type Max(type A, type B) { return A > B ? A : B; }

#if 0
#pragma push_macro("new")
#undef new
template<class type>
struct list {
    using iterator = type*;
    using const_iterator = const type*;
    using reverse_iterator = std::reverse_iterator<type*>;
    using reverse_const_iterator = std::reverse_iterator<const type*>;
    type* Begin = 0;
    type* End = 0;
    type* CapEnd = 0;
    list() = default;
    list(std::initializer_list<type> l) {
        Begin = (type*)malloc(l.size() * sizeof(type));
        CapEnd = End = Begin + l.size();
        CopyBuffer(Begin, l.begin(), l.size());
    }
    ~list() {
        clear();
        free(Begin);
    }
    void clear() {
        for (auto Src = Begin; Src < End; ++Src) {
            Src->~type();
        }
        End = Begin;
    }
    size_t count() const { return End - Begin; }
    size_t capacity() const { return CapEnd - Begin; }
    size_t remainingCapacity() const { return capacity() - count(); }
    void reallocate(size_t targetSize) {
        size_t newCapacity = capacity();
        if (newCapacity == 0)
            newCapacity = 1;
        while (newCapacity < targetSize)
            newCapacity *= 2;
        auto newBegin = (type*)malloc(newCapacity * sizeof(type));
        auto newEnd = newBegin + count();
        auto newCapEnd = newBegin + newCapacity;
        size_t i = 0;
        while (i < count()) {
            new(newBegin + i) type(std::move(Begin[i]));
            ++i;
        }
        clear();
        free(Begin);
        Begin = newBegin;
        End = newEnd;
        CapEnd = newCapEnd;
    }
    void ensureCapacity(size_t targetSize) {
        if (capacity() < targetSize)
            reallocate(targetSize);
    }
    template<class... args>
    type& emplace_back(args&&... Args) {
        ensureCapacity(count() + 1);
        return *new(End++) type(std::forward<args>(Args)...);
    }
    type& add(const type& val) {
        return emplace_back(val);
    }
    void insert(type* where, const list<type>& range) {
        assert(where >= Begin);
        assert(where <= End);
        if (capacity() < count() + range.size()) {
            auto targetSize = count() + range.size();
            size_t newCapacity = capacity();
            if (newCapacity == 0)
                newCapacity = 1;
            while (newCapacity < targetSize)
                newCapacity *= 2;
            auto newBegin = (type*)malloc(newCapacity * sizeof(type));
            auto newEnd = newBegin + count();
            auto newCapEnd = newBegin + newCapacity;
            type* Dst = newBegin;
            for (auto Src = Begin; Src < where; ) {
                new(Dst++) type(std::move(*Src++));
            }
            for (auto Src = range.Begin; Src < range.End; ) {
                new(Dst++) type(*Src++);
            }
            for (auto Src = where; Src < End; ) {
                new(Dst++) type(std::move(*Src++));
            }
            clear();
            free(Begin);
            Begin = newBegin;
            End = newEnd;
            CapEnd = newCapEnd;
        }
        else {
            auto Src = End - 1;
            auto Dst = Src + range.size();
            while (Src >= where) {
                new(Dst++) type(std::move(*Src++));
            }
            Dst = where;
            Src = range.Begin;
            while (Src < range.End) {
                new(Dst++) type(*Src++);
            }
        }
    }
    iterator begin() { return Begin; }
    iterator end() { return End; }
    const_iterator begin() const { return Begin; }
    const_iterator end() const { return End; }
    reverse_iterator rbegin() { return reverse_iterator(End); }
    reverse_iterator rend() { return reverse_iterator(Begin); }
    reverse_const_iterator rbegin() const { return reverse_const_iterator(End); }
    reverse_const_iterator rend() const { return reverse_const_iterator(Begin); }
    type& first() { return *Begin; }
    const type& first() const { return *Begin; }
    type& last() { return End[-1]; }
    const type& last() const { return End[-1]; }
    type& operator[](size_t i) { return Begin[i]; }
    const type& operator[](size_t i) const { return Begin[i]; }
};
#pragma pop_macro("new")
template<class type>
inline auto Reverse(list<type>& List) {
    return range(List.rbegin(), List.rend());
}
#else
template<class type>
using list = std::vector<type>;
#endif
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
template<class char_t>
struct basic_string_span {
    char_t* Begin = 0;
    char_t* End = 0;
    inline basic_string_span() {}
    inline basic_string_span(char_t* Begin, char_t* End) : Begin(Begin), End(End) {}
    inline basic_string_span(char_t* Strz) : basic_string_span(Strz, Strz + strlen(Strz)) {}
    inline basic_string_span(char_t* String, size_t Count) : basic_string_span(String, String + Count) {}
    inline basic_string_span(std::basic_string<char_t>& String) : basic_string_span(String.data(), String.data() + String.size()) {}
    inline size_t Count() const { return End - Begin; }
    inline char_t* begin() { return Begin; }
    inline char_t* end() { return End; }
    inline char_t& operator[](u32 I) { return Begin[I]; }
    inline char_t operator[](u32 I) const { return Begin[I]; }
};

template<class char_t>
struct basic_string_view;

template<class char_t, size_t Capacity>
struct basic_string_buffer {
    char_t Begin[Capacity];
    char_t* End = Begin;
    inline basic_string_buffer() {}
    inline basic_string_buffer(const basic_string_buffer& Rhs) {
        CopyBuffer(Begin, Rhs.Begin, Rhs.Count());
        End = Begin + Rhs.Count();
    }
    inline basic_string_buffer(char_t* B, char_t* E) : End(Begin + E - B) {
        CopyBuffer(Begin, B, End - Begin)
    }
    inline basic_string_buffer(char_t* Strz) : basic_string_buffer(Strz, Strz + StrLen(Strz)) {}
    inline basic_string_buffer(char_t* String, size_t Count) : basic_string_buffer(String, String + Count) {}
    inline basic_string_buffer(std::string& String) : basic_string_buffer(String.data(), String.data() + String.size()) {}
    inline basic_string_buffer(basic_string_span<char_t> Span) : basic_string_buffer(Span.Begin, Span.End) {}

    void Trim(size_t NewSize) {
        assert(NewSize < Count());
        End = Begin + NewSize;
    }
    size_t RemainingCapacity() {
        return Capacity - Count();
    }
    void Append(basic_string_view<char_t> Str) {
        assert(Str.Count() <= RemainingCapacity());
        CopyBuffer(End, Str.Begin, Str.Count());
        End += Str.Count();
    }
    void Append(const char_t* B, const char_t* E) {
        Append({B, E});
    }
    void Append(const char_t* B, size_t C) {
        Append({B, B + C});
    }
    inline size_t Count() const { return End - Begin; }
    inline char_t* begin() const { return Begin; }
    inline char_t* end() const { return End; }
    inline char_t& operator[](u32 I) { return Begin[I]; }
    inline char_t operator[](u32 I) const { return Begin[I]; }
};

template<class char_t>
struct basic_string_view {
    const char_t* Begin = 0;
    const char_t* End = 0;
    inline basic_string_view() {}
    inline basic_string_view(const char_t* Begin, const char_t* End) : Begin(Begin), End(End) {}
    inline basic_string_view(const char_t* Strz) : basic_string_view(Strz, Strz + strlen(Strz)) {}
    inline basic_string_view(const char_t* String, size_t Count) : basic_string_view(String, String + Count) {}
    inline basic_string_view(const std::string& String) : basic_string_view(String.data(), String.data() + String.size()) {}
    inline basic_string_view(basic_string_span<char_t> Span) : basic_string_view(Span.Begin, Span.End) {}
    template<size_t Capacity>
    inline basic_string_view(basic_string_buffer<char_t, Capacity> Buffer) : basic_string_view(Buffer.Begin, Buffer.End) {}

    inline size_t Count() const { return End - Begin; }
    inline const char_t* begin() const { return Begin; }
    inline const char_t* end() const { return End; }
    inline char_t operator[](u32 I) const { return Begin[I]; }
};

using string_span = basic_string_span<char>;
using wstring_span = basic_string_span<wchar>;

template<size_t Capacity = 256>
using string_buffer = basic_string_buffer<char, Capacity>;
template<size_t Capacity = 256>
using wstring_buffer = basic_string_buffer<wchar, Capacity>;

using string_view = basic_string_view<char>;
using wstring_view = basic_string_view<wchar>;

using directory_span = string_span;
using directory_buffer = string_buffer<MAX_PATH>;
using directory_view = string_view;

using directory_stdstring = std::string;

#define DIR_LITERAL(x) x

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
template<class t>
constexpr bool is_character = std::is_same_v<t, char> || std::is_same_v<t, wchar>;
template<class char_t, class = std::enable_if_t<is_character<char_t>>>
inline bool StringsAreEqualSafe(const char_t* A, const char_t* B, size_t Count) {
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

template<class char_t, size_t Capacity>
inline std::basic_string<char_t> ToString(const basic_string_buffer<char_t, Capacity>& String) {
    std::basic_string<char_t> Result;
    Result.resize(String.Count());
    CopyBuffer(Result.data(), String.Begin, String.Count());
    return Result;
}
template<class char_t>
inline std::basic_string<char_t> ToString(basic_string_span<char_t> String) {
    std::basic_string<char_t> Result;
    Result.resize(String.Count());
    CopyBuffer(Result.data(), String.Begin, String.Count());
    return Result;
}
template<class char_t>
inline std::basic_string<char_t> ToString(basic_string_view<char_t> String) {
    std::basic_string<char_t> Result;
    Result.resize(String.Count());
    CopyBuffer(Result.data(), String.Begin, String.Count());
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
    defer(callable&& C) : C(std::forward<callable>(C)) {}
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
inline size_t Append(message& Message, message::column Column) {
    Message.Column = Column;
    return 0;
}
inline size_t Append(message& Message, string_view Val) {
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
inline size_t Append(message& Message, const char* Val) {
    return Append(Message, string_view(Val));
}
inline size_t Append(message& Message, char* Val) {
    return Append(Message, (const char*)Val);
}
inline size_t Append(message& Message, const std::string& Val) {
    return Append(Message, string_view(Val));
}
inline size_t Append(message& Message, char Char) {
    return Append(Message, string_view(&Char, 1));
}
inline size_t Append(message& Message, u32 Val) {
    char Buf[10];
    ultoa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline size_t Append(message& Message, u64 Val) {
    char Buf[20];
    _ui64toa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline size_t Append(message& Message, s32 Val) {
    char Buf[11];
    itoa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline size_t Append(message& Message, s64 Val) {
    char Buf[21];
    _i64toa(Val, Buf, 10);
    return Append(Message, Buf);
}
inline size_t Append(message& Message, location Location) {
    char LineStr[16], ColumnStr[16];
    _itoa(Location.Line, LineStr, 10);
    _itoa(Location.Column, ColumnStr, 10);
    auto Column = Message.Column;
    Message.Column = {};
    size_t Result = 0;
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
inline size_t Append(message& Message, const type& Type, const types&... Types) {
    return Append(Message, Type) + Append(Message, Types...);
}
template<class uint, class = std::enable_if_t<std::is_unsigned_v<uint>>>
bool IsPowerOf2(uint Value) {
    bool Found = false;
    for (uint i = 0; i < sizeof(uint) * 8; ++i) {
        if (Value & ((uint)1 << i)) {
            if (Found)
                return false;
            Found = true;
        }
    }
    return true;
}
bool IsRelativePath(directory_view Path) {
    if (Path.Count()) {
        if (Path.Count() >= 2) {
            return Path[1] != L':';
        }
        else {
            return true;
        }
    }
    else {
        return true;
    }
}
directory_span Directory_RemoveDotDot(directory_span Dir) {
    for (auto C = Dir.Begin; C < Dir.End; ++C) {
        if (StringsAreEqualSafe(C, DIR_LITERAL("..\\"), 3)) {
            assert(C[-1] == '\\');
            auto D = C - 2;
            for (; D >= Dir.Begin; --D) {
                if (*D == '\\')
                    break;
            }
            ++D;
            StrCpy(D, C + 3);
            Dir.End = Dir.Begin + StrLen(Dir.Begin);
            C = D;
        }
    }
    return Dir;
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
