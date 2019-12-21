#pragma once
#include "common.h"
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
    std::vector<token> Tokens;
    std::vector<string_view> Lines;
    std::vector<std::string> ConvertedMultilineStringLiterals;
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
};

bool IsOverflowing(string_view Num) {
    string_view Max = "18446744073709551615";
    u32 MaxCount = Max.Count();
    if (Num.Count() > Max.Count())
        return true;
    else if (Num.Count() < Max.Count())
        return false;
    else {
        for (u32 i=0; i < MaxCount; ++i, Max.Begin++, Num.Begin++) {
            if (*Num.Begin > * Max.Begin)
                return true;
            else if (*Num.Begin < *Max.Begin)
                return true;
        }
        return false;
    }
}

#define LINE_FORMAT " %*u. "
void PrintMessages() {
    HANDLE ConsoleHandle = GetStdHandle(STD_OUTPUT_HANDLE);
    for (auto& Msg : GlobalCompiler->Messages) {
        puts(Msg.String);
        auto Loc = Msg.Location;
        if (Loc.Line) {
            u32 LineLength = (u32)log10(Loc.Line + 1) + 1;
            SetConsoleTextAttribute(ConsoleHandle, 0x08);
            if (Loc.LineView[0].Begin)
                printf("\n" LINE_FORMAT "%s\n", LineLength, Loc.Line - 1, ToString(Loc.LineView[0]).data());
            else
                puts("\n~~~ start of file ~~~");
            SetConsoleTextAttribute(ConsoleHandle, 0x07);
            printf(LINE_FORMAT "%s", LineLength, Loc.Line, std::string(Loc.LineView[1].Begin, Loc.TokenView.Begin).data());
            SetConsoleTextAttribute(ConsoleHandle, 0x47);
            printf("%s", ToString(Loc.TokenView).data());
            SetConsoleTextAttribute(ConsoleHandle, 0x07);
            printf("%s\n", std::string(Loc.TokenView.End, Loc.LineView[1].End).data());
            SetConsoleTextAttribute(ConsoleHandle, 0x08);
            if (Loc.LineView[2].Begin)
                printf(LINE_FORMAT "%s\n", LineLength, Loc.Line + 1, ToString(Loc.LineView[2]).data());
            else
                puts("~~~ end of file ~~~");
            SetConsoleTextAttribute(ConsoleHandle, 0x07);
            puts("");
        }
    }
}
#undef LINE_FORMAT