#pragma once
#include "..\common.h"
enum token_type : u8 {
    TokenType_Null,
    TokenType_Comment,
    TokenType_Literal,
    TokenType_Identifier,
    TokenType_Directive,
};
char* ToString(token_type Type) {
    switch (Type) {
        case TokenType_Null: return "Null";
        case TokenType_Comment: return "Comment";
        case TokenType_Literal: return "Literal";
        case TokenType_Identifier: return "Identifier";
        default: return "Unknown";
    }
}
struct token {
    string_view Value;
    token_type Type;
    data_type DataType;
    location Location;
    bool Multiline = false;
};

struct lexer {
    list<token> Tokens;
    list<string_view> Lines;
    list<std::string> ConvertedMultilineStringLiterals;
    b32 Success;
    void Process(string_view FileContents);
    bool NextToken();
    bool PrevToken();
    token PeekToken();
    bool Expect(string_view Str);
    bool ExpectStringLiteral();
    bool ExpectIntLiteral();
    bool ExpectIdentifier();

    u32 TokenIndex = 0;
    token CursorToken = {};
    size_t BytesUsed() {
        size_t Result =
            sizeof(*this) +
            Tokens.capacity() * sizeof(Tokens[0]) +
            Lines.capacity() * sizeof(Lines[0]) +
            ConvertedMultilineStringLiterals.capacity() * sizeof(ConvertedMultilineStringLiterals[0]);
        for (auto& Str : ConvertedMultilineStringLiterals)
            Result += Str.capacity() * sizeof(Str[0]);
        return Result;
    }
};

bool IsOverflowing(string_view Num) {
    string_view Max = "18446744073709551615";
    size_t MaxCount = Max.Count();
    if (Num.Count() > Max.Count())
        return true;
    else if (Num.Count() < Max.Count())
        return false;
    else {
        for (size_t i=0; i < MaxCount; ++i, Max.Begin++, Num.Begin++) {
            if (*Num.Begin > * Max.Begin)
                return true;
            else if (*Num.Begin < *Max.Begin)
                return true;
        }
        return false;
    }
}
