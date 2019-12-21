#include "lexer.h"
bool lexer::NextToken() {
    do {
        ++TokenIndex;
        if (TokenIndex >= Tokens.size()) {
            return false;
        }
        CursorToken = Tokens[TokenIndex];
    } while (CursorToken.Type == TokenType_Comment);
    return true;
}
bool lexer::PrevToken() {
    do {
        --TokenIndex;
        if (TokenIndex < 0) {
            return false;
        }
        CursorToken = Tokens[TokenIndex];
    } while (CursorToken.Type == TokenType_Comment);
    return true;
}
token lexer::PeekToken() {
    token Result = {};
    if (NextToken()) {
        Result = CursorToken;
        PrevToken();
    }
    return Result;
}

bool lexer::Expect(string_view Str) {
    if (CursorToken.Value != Str) {
        ReportError(CursorToken.Location, "Expected '", Str, "', got '", CursorToken.Value, "'");
        return false;
    }
    return true;
}
bool lexer::ExpectStringLiteral() {
    if (CursorToken.Type == TokenType_Literal) {
        if (CursorToken.DataType == data_type::String) {
            return true;
        }
        else {
            ReportError(CursorToken.Location, "Expected string literal, got ", ToString(CursorToken.DataType), " literal");
            return false;
        }
    }
    else {
        ReportError(CursorToken.Location, "Expected string literal, got ", ToString(CursorToken.Type));
        return false;
    }
}
bool lexer::ExpectIntLiteral() {
    if (CursorToken.Type == TokenType_Literal) {
        if (CursorToken.DataType == data_type::UInt) {
            return true;
        }
        else {
            ReportError(CursorToken.Location, "Expected int literal, got ", ToString(CursorToken.DataType), " literal");
            return false;
        }
    }
    else {
        ReportError(CursorToken.Location, "Expected int literal, got ", ToString(CursorToken.Type));
        return false;
    }
}
bool lexer::ExpectIdentifier() {
    if (CursorToken.Type == TokenType_Identifier) {
        return true;
    }
    else {
        ReportError(CursorToken.Location, "Expected identifier, got ", ToString(CursorToken.Type));
        return false;
    }
}
void lexer::Process(string_view FileContents) {
    if (FileContents.Count() == 0) {
        return;
    }

    string_view Line = {FileContents.Begin, FileContents.Begin};
    for (const char* C = FileContents.Begin; C < FileContents.End; ++C) {
        if (*C == '\n') {
            Lines.push_back(Line);
            Line.Begin = Line.End = C + 1;
        }
        else {
            ++Line.End;
        }
    }
    Lines.push_back(Line);

    const char* C = FileContents.Begin;
    token Token;
    location CurrentLocation = {{}, {}, {}, 1, 1};
    auto NextChar = [&]() {
        if (*C == '\n') {
            ++CurrentLocation.Line;
            CurrentLocation.Column = 0;
        }
        else if (*C == '\t') {
            CurrentLocation.Column += 3;
        }
        ++C;
        ++CurrentLocation.Column;
        return C != FileContents.End;
    };
    auto PushToken = [&]() {
        Token.Location.TokenView = Token.Value;
        if((int)Token.Location.Line - 2 >= 0)
            Token.Location.LineView[0] = Lines[Token.Location.Line - 2];
        Token.Location.LineView[1] = Lines[Token.Location.Line - 1];
        if(Token.Location.Line < Lines.size())
            Token.Location.LineView[2] = Lines[Token.Location.Line];
        Token.Location.FileView = GlobalCompiler->CompilingFilePaths.back();
        Tokens.push_back(std::move(Token));
    };
    auto ResetToken = [&]() {
        Token.Type = TokenType_Null;
        Token.DataType = data_type::Null;
        Token.Multiline = false;
        Token.Value.Begin = C;
        Token.Value.End = C;
        Token.Location = CurrentLocation;
    };
    while (*C) {
        if (C == FileContents.End)
            break;
        while (isspace(*C)) {
            if (!NextChar()) {
                goto Success;
            }
        }
        ResetToken();
        if (isalpha(*C) || *C == '_') {
            Token.Type = TokenType_Identifier;
            while (isalpha(*C) || isdigit(*C) || *C == '_') {
                ++Token.Value.End;
                NextChar();
            }
            if (Token.Value == "true" || Token.Value == "false") {
                Token.Type = TokenType_Literal;
                Token.DataType = data_type::Bool;
            }
            PushToken();
            continue;
        }
        else if (isdigit(*C)) {
            while (isdigit(*C)) {
                ++Token.Value.End;
                NextChar();
            }
            if (isalpha(*C)) {
                ReportError(Token.Location, "Suffix '", *C, "' for integer literal is not supported");
                goto Fail;
            }
            Token.Type = TokenType_Literal;
            Token.DataType = data_type::UInt;
            PushToken();
            if (IsOverflowing(Token.Value)) {
                ReportError(Token.Location, "Integer literal is too big");
                goto Fail;
            }
            continue;
        }
        else if (*C == '=' || *C == '!' || *C == '<' || *C == '>' || *C == '+' || *C == '-' || *C == '*') {
            ++Token.Value.End;
            NextChar();
            if (*C == '=') {
                ++Token.Value.End;
                NextChar();
            }
            PushToken();
        }
        else if (*C == '&') {
            ++Token.Value.End;
            NextChar();
            if (*C == '&') {
                ++Token.Value.End;
                NextChar();
            }
            PushToken();
        }
        else if (*C == '|') {
            ++Token.Value.End;
            NextChar();
            if (*C == '|') {
                ++Token.Value.End;
                NextChar();
            }
            PushToken();
        }
        else if (*C == '{' || *C == '}' || *C == '[' || *C == ']' || *C == '(' || *C == ')' || *C == '.' || *C == ',' || *C == ':' || *C == '%' || *C == '^' || *C == '@' || *C == '~') {
            ++Token.Value.End;
            NextChar();
            PushToken();
            continue;
        }
        else if (*C == '/') {
            token Beginning = Token;
            NextChar();
            if (*C == '/') {
                /* Single line comment */
                Token.Value.End += 2;
                NextChar();
                while (*C != '\n') {
                    ++Token.Value.End;
                    NextChar();
                }
                Token.Type = TokenType_Comment;
                PushToken();
                continue;
            }
            else if (*C == '*') {
                /* Multi line comment */
                Token.Value.End += 2;
                u32 CommentLevel = 1;
                /* */
                while (true) {
                    if (!NextChar()) {
                        ReportError(Beginning.Location, "Unclosed comment block");
                        goto Fail;
                    }
                    if (*C == '*') {
                        if (!NextChar()) {
                            ReportError(Beginning.Location, "Unclosed comment block");
                            goto Fail;
                        }
                        if (*C == '/') {
                            --CommentLevel;
                            Token.Value.End += 2;
                            if (!CommentLevel) {
                                NextChar();
                                break;
                            }
                        }
                    }
                    else if (*C == '/') {
                        if (!NextChar()) {
                            ReportError(Beginning.Location, "Unclosed comment block");
                            goto Fail;
                        }
                        if (*C == '*') {
                            ++CommentLevel;
                        }
                    }
                    ++Token.Value.End;
                }
                Token.Type = TokenType_Comment;
                PushToken();
                continue;
            }
            else {
                ++Token.Value.End;
                if (*C == '=') {
                    ++Token.Value.End;
                    NextChar();
                }
                PushToken();
                continue;
            }
        }
        else if (*C == '"') {
            auto Beginning = Token.Location;
            ++Token.Value.Begin;
            ++Token.Value.End;
            auto NextEnd = [&]() {
                if (!NextChar()) {
                    ReportError(Beginning, "Unclosed string literal (unexpected eof)");
                    return true;
                }
                return false;
            };
            if (NextEnd())
                goto Fail;
            if (*C == '\n') {
                ++Token.Value.Begin;
                ++Token.Value.End;
                if (NextEnd())
                    goto Fail;
            }
            while (*C != '"') {
                ++Token.Value.End;
                if (NextEnd())
                    goto Fail;
                if (*C == '\n') {
                    if (NextEnd())
                        goto Fail;
                    if (*C == '"') {
                        break;
                    }
                    else {
                        ++Token.Value.End;
                        Token.Multiline = true;
                    }
                }
            }
            Token.Type = TokenType_Literal;
            Token.DataType = data_type::String;
            PushToken();
            if (!NextChar()) {
                goto Success;
            }
            continue;
        }
        else if (*C == '\'') {
            auto Beginning = Token.Location;
            ++Token.Value.Begin;
            ++Token.Value.End;
            auto CheckError = [&](bool Cond, string_view Message) {
                if (!Cond) {
                    Beginning.TokenView = Token.Value;
                    Beginning.TokenView.Begin--;
                    while (*Beginning.TokenView.End != '\'' && *Beginning.TokenView.End != '\n')
                        ++Beginning.TokenView.End;
                    if (*Beginning.TokenView.End == '\'')
                        ++Beginning.TokenView.End;
                    ReportError(Beginning, Message);
                }
                return !Cond;
            };
            if (CheckError(NextChar(), "Unclosed char literal (unexpected eof)"))
                goto Fail;
            char FirstChar = *C;
            ++Token.Value.End;
            if (CheckError(NextChar(), "Unclosed char literal (unexpected eof)"))
                goto Fail;
            if (FirstChar == '\\') {
                ++Token.Value.End;
                if (CheckError(NextChar(), "Unclosed char literal (unexpected eof)"))
                    goto Fail;
            }
            if (CheckError(*C == '\'', "Too many characters in char literal"))
                goto Fail;
            Token.Type = TokenType_Literal;
            Token.DataType = data_type::Char;
            PushToken();
            if (!NextChar()) {
                goto Fail;
            }
            continue;
        }
        else if (*C == '#') {
            NextChar();
            ++Token.Value.End;
            Token.Type = TokenType_Directive;
            if (isalpha(*C)) {
                while (isalpha(*C) || isdigit(*C) || *C == '_') {
                    ++Token.Value.End;
                    NextChar();
                }
#if 0
                if (Token.Value == "#begin_code") {
                    while (isspace(*C))
                        NextChar();

                    Token.Value.Begin = Token.Value.End = C;
                    while (isalpha(*C)) {
                        ++Token.Value.End;
                        NextChar();
                    }
                    Token.Type = TokenType_InlineCode;
                    PushToken();
                    ResetToken();
                    while (1) {
                        if (StringsAreEqualSafe(C, "#end_code", 9)) {
                            break;
                        }
                        ++Token.Value.End;
                        NextChar();
                    }
                    PushToken();
                    C += 9;
                    continue;
                }
#endif
                PushToken();
                continue;
            }
            else {
                ReportError(Token.Location, "Unexpected '", *C, "'");
            }
        }
        else {
            ++Token.Value.End;
            PushToken();
            ReportError(Token.Location, "wtf is '", *C, "'?");
            goto Fail;
        }
    }
    goto Success;
Fail:
    Success = false;
Success:
    for (auto& T : Tokens) {
        if (T.Multiline) {
            string_builder Builder;
            for (const char* Ch = T.Value.Begin; Ch < T.Value.End; ++Ch) {
                if (*Ch == '\n')
                    Builder.Append("\\n");
                else
                    Builder.Append(*Ch);
            }
            ConvertedMultilineStringLiterals.push_back(Builder.GetString());
            T.Value = T.Location.TokenView = ConvertedMultilineStringLiterals.back();
        }
    }
    CursorToken = Tokens[0];
    if (CursorToken.Type == TokenType_Comment) {
        NextToken();
    }
}