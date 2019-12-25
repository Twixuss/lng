#include "..\compiler.h"
#include "..\translate.h"
#include "parser.h"

void compiler::Compile() {
    _ReportMessage = [](compiler* C, const message& Msg) { C->Messages.push_back(Msg); };

    Instance = this;

    directory_buffer FileDir;
    if (IsRelativePath(SourceFileName)) {
        FileDir.Append(CurrentDirectory.Begin, CurrentDirectory.Count());
        FileDir.Append(SourceFileName.Begin, SourceFileName.Count());
    }
    else {
        FileDir.Append(SourceFileName.Begin, SourceFileName.Count());
    }
    for (auto C = FileDir.Begin + FileDir.Count() - 1; C > FileDir.Begin; --C) {
        if (*C == '\\') {
            C[1] = 0;
            break;
        }
    }
    FileDir.Trim(Directory_RemoveDotDot(FileDir.Begin).Count());

    printf("FileDir: %s\n", FileDir.Begin);

    directory_buffer CompiledFilePathBuffer;
    CopyBuffer(CompiledFilePathBuffer.Begin, CurrentDirectory.Begin, CurrentDirectory.Count());
    CopyBuffer(CompiledFilePathBuffer.Begin + CurrentDirectory.Count(), SourceFileName.Begin, SourceFileName.Count());
    CompilingFilePaths.push_back({CompiledFilePathBuffer.Begin, CurrentDirectory.Count() + SourceFileName.Count()});

    ExitCode = COMPILE_SUCCESS;
    Lexer = new lexer;
    Lexer->Process(Source);

    if (Lexer->Success) {
        directory_buffer LogFileName;
        CopyBuffer(LogFileName.Begin, SourceFileName.Begin, SourceFileName.Count());
        StrCpy(LogFileName.Begin + SourceFileName.Count(), ".tokens");
        FILE* TokensFile = fopen(LogFileName.Begin, "wb");
        if (TokensFile) {
            char* Format = "%-40s%-15s%-10s\n";
            fprintf(TokensFile, Format, "Value", "AstType", "Data");
            for (token Token : Lexer->Tokens) {
                fprintf(TokensFile, Format, ToString(Token.Value).data(), ToString(Token.Type), ToString(Token.DataType));
            }
        }
        else {
            puts("Can't open tokens log file!");
        }

        Parser = new parser(Lexer);
        Parser->Run(this);
        if (Parser->Success) {
            if (Parser->TempDir.starts_with("<filedir>")) {
                Parser->TempDir.replace(Parser->TempDir.begin(), Parser->TempDir.begin() + 9, FileDir.Begin, FileDir.End);
            }
            if (IsRelativePath(Parser->TempDir)) {
                sprintf(TempDir, "%s%s", CurrentDirectory.Begin, Parser->TempDir.data());
            }
            else {
                strcpy(TempDir, Parser->TempDir.data());
            }
            if (Parser->BinDir.starts_with("<filedir>")) {
                Parser->BinDir.replace(Parser->BinDir.begin(), Parser->BinDir.begin() + 9, FileDir.Begin, FileDir.End);
            }
            if (IsRelativePath(Parser->BinDir)) {
                sprintf(BinDir, "%s%s", CurrentDirectory.Begin, Parser->BinDir.data());
            }
            else {
                strcpy(BinDir, Parser->BinDir.data());
            }
            if (!CreateDirectoryA(TempDir, 0)) { assert(GetLastError() == ERROR_ALREADY_EXISTS); }
            if (!CreateDirectoryA(BinDir, 0)) { assert(GetLastError() == ERROR_ALREADY_EXISTS); }
            char TranslatorFile[MAX_PATH];
            sprintf(TranslatorFile, "%s\\translate.dll", ToString(Parser->TranslatorName).data());
            HMODULE Translator = LoadLibraryA(TranslatorFile);
            if (Translator) {
                using translate = int(*)(compiler*);
                translate Translate = (translate)GetProcAddress(Translator, "_Translate");
                assert(Translate);
                if (Translate(this) != TRANSLATE_OK) {
                    puts("Translation failed");
                    ExitCode = COMPILE_ERROR;
                }
                FreeLibrary(Translator);
            }
            else {
                printf("Can't open %s\n", TranslatorFile);
                ExitCode = COMPILE_ERROR;
            }
#if PRINT_AST
            if (Parser->GlobalNode->Declarations.size() > 0) {
                strcpy(LogFileName + SourceFileName.Count(), ".ast");
                FILE* ASTFile = fopen(LogFileName, "wb");
                if (ASTFile) {
                    for (node* Node : Parser->GlobalNode->Declarations) {
                        PrintNode(ASTFile, Node);
                    }
                    fclose(ASTFile);
                }
                else {
                    puts("Can't open ast log file!");
                    ExitCode = COMPILE_ERROR;
                }
            }
#endif
        }
        else {
            puts("Parsing failed");
            ExitCode = COMPILE_ERROR;
        }
        if (TokensFile) {
            fclose(TokensFile);
        }
    }
    else {
        puts("Lexing failed");
        ExitCode = COMPILE_ERROR;
    }
}
void compiler::Free() {
    f64 BytesUsed = 
        sizeof(*this) +
        CompilingFilePaths.capacity() * sizeof(CompilingFilePaths[0]);
    for (auto& Str : CompilingFilePaths)
        BytesUsed += Str.capacity() * sizeof(Str[0]);

    if (Parser) {
        BytesUsed += Parser->BytesUsed();
        Parser->Free();
        delete Parser;
    }
    if (Lexer) {
        BytesUsed += Lexer->BytesUsed();
        //Lexer->Free();
        delete Lexer;
    }
    for (auto Ptr : FilesContents)
        delete Ptr;

    const char* Units[] {
        "B", 
        "KB",
        "MB",
        "GB",
        "TB",
        "PB",
    };
    auto Unit = Units;

    while (BytesUsed > 1024) {
        BytesUsed /= 1024;
        ++Unit;
    }
    printf("Memory used: %.3f %s\n", BytesUsed, *Unit);
    Instance = 0;
}

#define LINE_FORMAT " %*u. "
void compiler::PrintMessages() {
    HANDLE ConsoleHandle = GetStdHandle(STD_OUTPUT_HANDLE);
    for (auto& Msg : Messages) {
        puts(Msg.String);
        auto Loc = Msg.Location;
        if (Loc.Line) {
            u32 LineLength = (u32)log10(Loc.Line + 1) + 1;
            //SetConsoleTextAttribute(ConsoleHandle, 0x08);
            //if (Loc.LineView[0].Begin)
            //    printf("\n" LINE_FORMAT "%s\n", LineLength, Loc.Line - 1, ToString(Loc.LineView[0]).data());
            //else
            //    puts("\n~~~ start of file ~~~");
            //SetConsoleTextAttribute(ConsoleHandle, 0x07);
            printf(LINE_FORMAT "%s", LineLength, Loc.Line, std::string(Loc.LineView[1].Begin, Loc.TokenView.Begin).data());
            SetConsoleTextAttribute(ConsoleHandle, 0x47);
            printf("%s", ToString(Loc.TokenView).data());
            SetConsoleTextAttribute(ConsoleHandle, 0x07);
            printf("%s\n", std::string(Loc.TokenView.End, Loc.LineView[1].End).data());
            //SetConsoleTextAttribute(ConsoleHandle, 0x08);
            //if (Loc.LineView[2].Begin)
            //    printf(LINE_FORMAT "%s\n", LineLength, Loc.Line + 1, ToString(Loc.LineView[2]).data());
            //else
            //    puts("~~~ end of file ~~~");
            //SetConsoleTextAttribute(ConsoleHandle, 0x07);
            puts("");
        }
    }
}
#undef LINE_FORMAT
