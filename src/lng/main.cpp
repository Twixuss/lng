#include <malloc.h>

#include <functional>
#include <stack>

#include "..\common.h"
#include "..\translate.h"

#include "parser.cpp"
#include "lexer.cpp"
#include "compiler.cpp"

#define PRINT_AST 1
#define WRITE_AST 0

u32 IndentLevel;
template<class... types>
void AST_Printf(FILE* File, char* Format, const types&... Types) {
    for (u32 I = 0; I < IndentLevel; ++I) {
        fprintf(File, "    ");
    }
    fprintf(File, Format, Types...);
}

#if PRINT_AST
void PrintNodeRef(FILE* File, node_typedecl* TypeDecl) {
    if (TypeDecl) {
        AST_Printf(File, "Type: 0x%p (Name: %s)\n", TypeDecl, ToString(TypeDecl->Identifier).data());
    }
    else {
        AST_Printf(File, "Type: null\n");
    }
}
void PrintNodeRef(FILE* File, node_vardecl* VarDecl) {
    if (VarDecl) {
        AST_Printf(File, "VarDecl: 0x%p (Name: %s)\n", VarDecl, ToString(VarDecl->Identifier).data());
    }
    else {
        AST_Printf(File, "VarDecl: null\n");
    }
}
void PrintNodeRef(FILE* File, node_functiondecl* FuncDecl) {
    if (FuncDecl) {
        AST_Printf(File, "FuncDecl: 0x%p (Name: %s)\n", FuncDecl, ToString(FuncDecl->Identifier).data());
    }
    else {
        AST_Printf(File, "FuncDecl: null\n");
    }
}
void PrintNodeRef(FILE* File, node_nsdecl* NSDecl) {
    if (NSDecl) {
        AST_Printf(File, "NamespaceDecl: 0x%p (Name: %s)\n", NSDecl, ToString(NSDecl->Identifier).data());
    }
    else {
        AST_Printf(File, "NamespaceDecl: null\n");
    }
}
void PrintNodeRef(FILE* File, node_membervardecl* Member) {
    if (Member) {
        AST_Printf(File, "Member: 0x%p (Name: %s)\n", Member, ToString(Member->Declaration->Identifier).data());
    }
    else {
        AST_Printf(File, "Member: null\n");
    }
}
void PrintNodeRef(FILE* File, node_type* Type) {
    AST_Printf(File, "Type: %s\n", ToString(Type).data());
}
void PrintNodeRef(FILE* File, node* Node) {
    switch (Node->AstType) {
        case NodeType_TypeDecl: PrintNodeRef(File, (node_typedecl*)Node); break;
        case NodeType_VarDecl: PrintNodeRef(File, (node_vardecl*)Node); break;
        case NodeType_FunctionDecl: PrintNodeRef(File, (node_functiondecl*)Node); break;
        case NodeType_MemberVarDecl: PrintNodeRef(File, (node_membervardecl*)Node); break;
        case NodeType_NSDecl: PrintNodeRef(File, (node_nsdecl*)Node); break;
        default: assert(0);
    }
}
void PrintNode(FILE* File, node* Node) {
    assert(Node);
    switch (Node->AstType) {
        case NodeType_Assignment: {
            AST_Printf(File, "Assignment: 0x%p\n", Node);
            node_assignment& Assignment = *(node_assignment*)Node;
            ++IndentLevel;
            AST_Printf(File, "Assigned:\n");
            ++IndentLevel;
            PrintNode(File, Assignment.Assigned);
            --IndentLevel;
            AST_Printf(File, "Expression:\n");
            ++IndentLevel;
            PrintNode(File, Assignment.Expression);
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_Literal: {
            AST_Printf(File, "Literal: 0x%p\n", Node);
            node_literal& Literal= *(node_literal*)Node;
            ++IndentLevel;
            switch (Literal.LiteralType) {
                case data_type::String:
                    AST_Printf(File, "Value: \"%s\"\n", ToString(Literal.Value.Str).data());
                    break;
                case data_type::Char:
                    AST_Printf(File, "Value: '%s'\n", ToString(Literal.Value.Str).data());
                    break;
                case data_type::UInt:
                    AST_Printf(File, "Value: %s\n", std::to_string(Literal.Value.Int).data());
                    break;
                default:
                    AST_Printf(File, "Value: %s\n", ToString(Literal.Value.Str).data());
                    break;
            }--IndentLevel;
            break;
        }
        case NodeType_VarDecl: {
            AST_Printf(File, "VarDecl: 0x%p\n", Node);
            node_vardecl& VarDecl = *(node_vardecl*)Node;
            ++IndentLevel;
            AST_Printf(File, "Name: %s\n", ToString(VarDecl.Identifier).data());
            PrintNodeRef(File, VarDecl.Type);
            if (VarDecl.InitialExpression) {
                AST_Printf(File, "Expression:\n");
                ++IndentLevel;
                PrintNode(File, VarDecl.InitialExpression);
                --IndentLevel;
            }
            --IndentLevel;
            break;
        }
        case NodeType_FunctionDecl: {
            AST_Printf(File, "FunctionDecl: 0x%p\n", Node);
            node_functiondecl& FuncDecl = *(node_functiondecl*)Node;
            ++IndentLevel;
            AST_Printf(File, "Name: %s\n", ToString(FuncDecl.Identifier).data());
            AST_Printf(File, "Arguments:\n");
            ++IndentLevel;
            for (node_vardecl* Param : FuncDecl.Arguments) {
                PrintNode(File, Param);
            }
            --IndentLevel;
            AST_Printf(File, "Statements:\n");
            ++IndentLevel;
            for (node* N : FuncDecl.Statements) {
                PrintNode(File, N);
            }
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_FunctionCall: {
            AST_Printf(File, "FunctionCall: 0x%p\n", Node);
            node_functioncall& Call= *(node_functioncall*)Node;
            ++IndentLevel;
            AST_Printf(File, "Name: %s\n", ToString(Call.Identifier).data());
            PrintNodeRef(File, Call.Declaration);
            AST_Printf(File, "Arguments:\n");
            ++IndentLevel;
            for (node* N : Call.Arguments) {
                PrintNode(File, N);
            }
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_TypeDecl: {
            AST_Printf(File, "TypeDecl: 0x%p\n", Node);
            node_typedecl& Decl = *(node_typedecl*)Node;
            ++IndentLevel;
            AST_Printf(File, "Name: %s\n", ToString(Decl.Identifier).data());
            AST_Printf(File, "Size: %u\n", Decl.Size);
            AST_Printf(File, "Align: %u\n", Decl.Align.Value);
            AST_Printf(File, "Members:\n");
            ++IndentLevel;
            for (node* Member : Decl.Members) {
                switch (Member->AstType) {
                    case NodeType_MemberVarDecl: {
                        auto Var = (node_membervardecl*)Member;
                        AST_Printf(File, "Member variable:\n");
                        ++IndentLevel;
                        AST_Printf(File, "Offset: %u\n", Var->Offset.Value);
                        PrintNode(File, Var->Declaration);
                        --IndentLevel;
                        break;
                    }
                    case NodeType_MemberFuncDecl: {
                        auto Func = (node_memberfuncdecl*)Member;
                        AST_Printf(File, "Member function:\n");
                        ++IndentLevel;
                        PrintNode(File, Func->Declaration);
                        --IndentLevel;
                        break;
                    }
                }
            }
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_TypeAlias: {
            AST_Printf(File, "TypeAlias: 0x%p\n", Node);
            node_typealias& Decl = *(node_typealias*)Node;
            ++IndentLevel;
            AST_Printf(File, "New name: %s\n", ToString(Decl.NewIdentifier).data());
            AST_Printf(File, "Old name: %s\n", ToString(Decl.OldIdentifier).data());
            --IndentLevel;
            break;
        }
        case NodeType_Identifier: {
            node_identifier* Identifier = (node_identifier*)Node;
            AST_Printf(File, "Identifier: %s\n", ToString(Identifier->Identifier).data());
            ++IndentLevel;
            PrintNodeRef(File, Identifier->Declaration);
            --IndentLevel;
            break;
        }
        case NodeType_MemberAccess: {
            AST_Printf(File, "MemberAccess: 0x%p\n", Node);
            node_memberaccess* MemberAccess = (node_memberaccess*)Node;
            ++IndentLevel;
            PrintNode(File, MemberAccess->First);
            PrintNode(File, MemberAccess->Second);
            --IndentLevel;
            break;
        }
        case NodeType_Sizeof: {
            AST_Printf(File, "Sizeof: 0x%p\n", Node);
            node_sizeof& Sizeof = *(node_sizeof*)Node;
            ++IndentLevel;
            AST_Printf(File, "Type: %s\n", ToString(Sizeof.Type).data());
            --IndentLevel;
            break;
        }
        case NodeType_Paren: {
            AST_Printf(File, "Paren: 0x%p\n", Node);
            ++IndentLevel;
            PrintNode(File, ((node_paren*)Node)->Expression);
            --IndentLevel;
            break;
        }
        case NodeType_Return: {
            AST_Printf(File, "Return: 0x%p\n", Node);
            node_return& Return = *(node_return*)Node;
            ++IndentLevel;
            PrintNode(File, Return.Expression);
            --IndentLevel;
            break;
        }
        case NodeType_If: {
            AST_Printf(File, "If: 0x%p\n", Node);
            node_if& If = *(node_if*)Node;
            ++IndentLevel;
            AST_Printf(File, "Condition:\n");
            ++IndentLevel;
            PrintNode(File, If.Condition);
            --IndentLevel;
            AST_Printf(File, "Statements:\n");
            ++IndentLevel;
            for (node* N : If.Statements) {
                PrintNode(File, N);
            }
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_NSDecl: {
            AST_Printf(File, "Namespace Decl: 0x%p\n", Node);
            node_nsdecl& NSDecl = *(node_nsdecl*)Node;
            ++IndentLevel;
            AST_Printf(File, "Identifier: %s\n", ToString(NSDecl.Identifier).data());
            AST_Printf(File, "Declarations:\n");
            ++IndentLevel;
            for (node* Node : NSDecl.Declarations) {
                PrintNode(File, Node);
            }
            --IndentLevel;
            --IndentLevel;
            break;
        }
        case NodeType_BinaryOp: {
            AST_Printf(File, "BinaryOp: 0x%p\n", Node);
            node_binaryop& BinaryOp = *(node_binaryop*)Node;
            ++IndentLevel;
            AST_Printf(File, "Operation: %s\n", ToString(BinaryOp.Op).data());
            if (BinaryOp.Declaration)
                PrintNodeRef(File, BinaryOp.Declaration);
            AST_Printf(File, "FirstExpression:\n");
            ++IndentLevel;
            PrintNode(File, BinaryOp.FirstExpression);
            --IndentLevel;
            AST_Printf(File, "SecondExpression:\n");
            ++IndentLevel;
            PrintNode(File, BinaryOp.SecondExpression);
            --IndentLevel;
            --IndentLevel;
            break;
        }
        default: {
            AST_Printf(File, "Unknown node. AstType: %s\n", ToString(Node->AstType));
            break;
        }
    }
};
#endif
inline std::string Convert_CRLF_LF(string_view String) {
    string_builder Builder;
    for (char C : String) {
        if (C != '\r')
            Builder.Append(C);
    }
    return Builder.GetString();
}

#if WRITE_AST

#define NULL_ID (u32)-1

template<class type>
auto Read(char*& Cursor) {
    type Result = *(type*)Cursor; // BUG there was break
    Cursor += sizeof(type);
    return Result;
}
void ReadNode(FILE* File, char*& Cursor) {
    ast_type Type = Read<ast_type>(Cursor);
    u32 ID = Read<u32>(Cursor);
    u32 ParentID = Read<u32>(Cursor);
    AST_Printf(File, "AstType: %s\n", ToString(Type));
    ++IndentLevel;
    AST_Printf(File, "ID: %u\n", ID);
    if (ParentID == NULL_ID)
        AST_Printf(File, "ParentID: null\n");
    else
        AST_Printf(File, "ParentID: %u\n", ParentID);
    switch (Type) {
        case NodeType_VarDecl: {
            u32 TypeID = Read<u32>(Cursor);
            u8 HasInitialExpression = Read<u8>(Cursor);
            if (HasInitialExpression) {
                AST_Printf(File, "Initial expression:\n");
                ++IndentLevel;
                ReadNode(File, Cursor);
                --IndentLevel;
            }
            u16 IdentifierSize = Read<u16>(Cursor);
            AST_Printf(File, "TypeID: %u\n", TypeID);
            AST_Printf(File, "Identifier: %s\n", std::string(Cursor, Cursor + IdentifierSize).data());
            Cursor += IdentifierSize;
            break;
        }
        case NodeType_FunctionDecl: {
            u16 IdentifierSize = Read<u16>(Cursor);
            AST_Printf(File, "Identifier: %s\n", std::string(Cursor, Cursor + IdentifierSize).data());
            Cursor += IdentifierSize;
            u32 StatementCount = Read<u32>(Cursor);
            ++IndentLevel;
            for (u32 I = 0; I < StatementCount; ++I) {
                ReadNode(File, Cursor);
            }
            --IndentLevel;
            break;
        }
        default:
            //puts("Unknown node type");
            //assert(false);
            break;
    }
    --IndentLevel;
}
void ReadTest(char* FileName) {
    FILE* File = fopen(FileName, "rb");
    FILE* OutFile = fopen("read_test", "wb");
    if (File) {
        if (OutFile) {
            fseek(File, 0, SEEK_END);
            size_t FileSize = ftell(File);
            fseek(File, 0, SEEK_SET);
            char* FileContents = (char*)malloc(FileSize);
            fread(FileContents, FileSize, 1, File);

            char* Cursor = FileContents;
            assert(string_view(Cursor, 4) == "lng_");
            Cursor += 4;
            u32 Version = Read<u32>(Cursor);
            fprintf(OutFile, "Version: %u\n", Version);

            IndentLevel = 0;

            u32 DeclarationCount = Read<u32>(Cursor);
            for (u32 I = 0; I < DeclarationCount; ++I) {
                ReadNode(OutFile, Cursor);
            }
            free(FileContents);
            fclose(OutFile);
        }
        else {
            puts("Failed to open test file!");
        }
        fclose(File);
    }
    else {
        puts("Failed to read results!");
    }
}
void WriteNode(string_builder& Builder, node* Node) {
    Builder.AppendBinary(Node->AstType);
    Builder.AppendBinary(Node->ID);
    Builder.AppendBinary(Node->Parent->ID);
    switch (Node->AstType) {
        case NodeType_VarDecl: {
            node_vardecl& VarDecl = *(node_vardecl*)Node;
            Builder.AppendBinary(VarDecl.Type.Decl->ID);
            if (VarDecl.InitialExpression) {
                Builder.AppendBinary<u8>(255);
                WriteNode(Builder, VarDecl.InitialExpression);
            }
            else {
                Builder.AppendBinary<u8>(0);
            }
            Builder.AppendBinary((u16)VarDecl.Identifier.Count());
            Builder.Append(VarDecl.Identifier);
            break;
        }
        case NodeType_FunctionDecl: {
            node_functiondecl& FuncDecl = *(node_functiondecl*)Node;
            Builder.AppendBinary((u16)FuncDecl.Identifier.Count());
            Builder.Append(FuncDecl.Identifier);
            Builder.AppendBinary((u32)FuncDecl.Statements.size());
            for (node* Statement : FuncDecl.Statements) {
                WriteNode(Builder, Statement);
            }
            break;
        }
        default:
            //puts("Unknown node type");
            //assert(false);
            break;
    }
};
void WriteToFile(compiler& Result, char* FileName) {
    FILE* File = fopen(FileName, "wb");
    if (File) {
        string_builder Builder;

        u32 Version = 0;

        Builder.Append("lng_");
        Builder.AppendBinary(Version);
        Builder.AppendBinary((u32)Result.Parser->GlobalNode->Declarations.size());

        for (node* Node : Result.Parser->GlobalNode->Declarations) {
            WriteNode(Builder, Node);
        }

        std::string FileContents = Builder.GetString();
        fwrite(FileContents.data(), FileContents.size(), 1, File);
        fclose(File);

        ReadTest(FileName);
    }
    else {
        puts("Failed to write results!");
    }
}

#endif

FILETIME GetLastWriteTime(char* Filename) {
    FILETIME Result = {};

    WIN32_FILE_ATTRIBUTE_DATA FileInfo;

    if (GetFileAttributesExA(Filename, GetFileExInfoStandard, &FileInfo)) {
        Result = FileInfo.ftLastWriteTime;
    }
    else {
        printf("%s: Cant get last write time.\n", Filename);
    }
    return Result;
}
int main(int ArgumentCount, char* Arguments[]) {
    puts("Arguments");
    for (int i = 0; i < ArgumentCount; ++i) {
        puts(Arguments[i]);
    }
    puts("");
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDOUT);
    int Result = 0;
    bool RecompileOnUpdate = false;
    char* SourceFileName = 0;
    //char* OutputFile = "a.lngout";
    char* TranslatorFile = 0;
    for (int I = 1; I < ArgumentCount; ++I) {
        char* Arg = Arguments[I];
        if (Arg[0] == '-') {
            char* Option = Arg + 1;
            /* Recompile on update */
            if (StringsAreEqualSafe(Option, "ru", 2)) {
                puts("Recompile on update: yes");
                RecompileOnUpdate = true;
            }
            //else if (StringsAreEqualSafe(Option, "out", 3)) {
            //    OutputFile = Arguments[++I];
            //    printf("Output file: %s\n", OutputFile);
            //}
            else if (StringsAreEqualSafe(Option, "translate", 3)) {
                TranslatorFile = Arguments[++I];
                printf("Translator file: %s\n", TranslatorFile);
            }
        }
        else {
            SourceFileName = Arg;
        }
    }
    directory_buffer CurrentDirectoryBuf;
    DWORD CurrentDirSize = GetCurrentDirectoryA(COUNT_OF(CurrentDirectoryBuf.Begin), CurrentDirectoryBuf.Begin);
    if (CurrentDirectoryBuf[CurrentDirSize - 1] != '\\') {
        CurrentDirectoryBuf[CurrentDirSize++] = '\\';
        CurrentDirectoryBuf[CurrentDirSize] = 0;
    }
    printf("CurrentDirectory: %s\n", CurrentDirectoryBuf.Begin);
    directory_view CurrentDirectory = {CurrentDirectoryBuf.Begin, CurrentDirSize};

    string_view CompilerPath = Arguments[0];
    size_t CompilerPathSize = 0;
    directory_buffer CompilerDirectoryBuf;
    if (IsRelativePath(CompilerPath)) {
        CopyBuffer(CompilerDirectoryBuf.Begin, CurrentDirectory.Begin, CurrentDirectory.Count());
        CopyBuffer(CompilerDirectoryBuf.Begin + CurrentDirectory.Count(), CompilerPath.Begin, CompilerPath.Count());
        CompilerPathSize = CurrentDirectory.Count() + CompilerPath.Count();
    }
    else {
        CopyBuffer(CompilerDirectoryBuf.Begin, CompilerPath.Begin, CompilerPath.Count());
        CompilerPathSize = CompilerPath.Count();
    }
    directory_view CompilerDirectory;
    CompilerDirectory.Begin = CompilerDirectoryBuf.Begin;
    for (auto C = CompilerDirectoryBuf.Begin + CompilerPathSize - 1; C > CompilerDirectoryBuf.Begin; --C) {
        if (*C == '\\') {
            C[1] = 0;
            CompilerDirectory.End = C + 1;
            break;
        }
    }
    printf("CompilerDirectory: %s\n", CompilerDirectoryBuf.Begin);

    auto CompileAndPrint = [&]() {
        FILE* File = fopen(SourceFileName, "rb");
        if (File) {
            printf("Compiling: %s\n", SourceFileName);
            size_t FileSize = 0;
            fseek(File, 0, SEEK_END);
            FileSize = ftell(File);
            fseek(File, 0, SEEK_SET);

            string_span FileContents = {(char*)malloc(FileSize), FileSize};
            fread(FileContents.Begin, FileSize, 1, File);

            auto ValidFileContents = Convert_CRLF_LF(FileContents);

            compiler Compiler;
            Compiler.Source = ValidFileContents;
            Compiler.SourceFileName = SourceFileName;
            Compiler.CurrentDirectory = CurrentDirectory;
            Compiler.CompilerDirectory = CompilerDirectory;
            {
                //LEAK_CHECKER("Compile");
                Compiler.Compile();
            }

            switch (Compiler.ExitCode) {
                case COMPILE_SUCCESS:
                    puts("Compilation succeeded.");
                    break;
                case COMPILE_ERROR:
                    Compiler.PrintMessages();
                    puts("Compilation failed.");
                    break;
            }

            Compiler.Free();
            free(FileContents.Begin);

            fclose(File);
            return true;
        }
        else {
            printf("File not found: %s", SourceFileName);
            return false;
        }
    };
    if (SourceFileName) {
        if (RecompileOnUpdate) {
            FILETIME LastWriteTime = {};
            while (true) {
                FILETIME WriteTime = GetLastWriteTime(SourceFileName);
                if (!WriteTime.dwLowDateTime) {
                    puts("Can't open source file!");
                    return 1;
                }
                if (CompareFileTime(&WriteTime, &LastWriteTime) != 0) {
                    puts("");
                    LastWriteTime = WriteTime;

                    if (!CompileAndPrint()) {
                        break;
                    }
                    puts("Waiting for file update");
                }
                Sleep(100);
            }
        }
        else {
            CompileAndPrint();
        }
    }
    else {
        puts("No source file provided!");
    }
    return Result;
}