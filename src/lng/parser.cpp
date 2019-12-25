#include "..\compiler.h"
#include "parser.h"
#include <algorithm>

#define NEXT_TOKEN_OK_FINISH return NextToken() ? PARSE_OK : PARSE_OK_FINISH

void parser::Free() {
    auto Blk = Memory.FirstBlock;
    while (Blk) {
        auto Del = Blk;
        Blk = Del->Next;
        VirtualFree(Del, 0, MEM_RELEASE);
    }
    if (GlobalScope)
        FreeScope(GlobalScope);
}
void parser::Run(compiler* Compiler) {
    if (Lexer->Tokens.size() == 0) {
        return;
    }
    GlobalNode = CreateGlobalNode();

    GlobalScope = CurrentScope = new scope;

    auto CreateBuiltInType = [&](string_view Name, u32 Size) {
        return GlobalScope->Types[Name] = CreateTypeDecl(GlobalNode, {}, Name, Size);
    };

    Type_void    = CreateBuiltInType("void", 0);
    Type_bool    = CreateBuiltInType("bool", 1);
    Type_int8    = CreateBuiltInType("int8", 1);
    Type_int16   = CreateBuiltInType("int16", 2);
    Type_int32   = CreateBuiltInType("int32", 4);
    Type_int64   = CreateBuiltInType("int64", 8);
    Type_uint8   = CreateBuiltInType("uint8", 1);
    Type_uint16  = CreateBuiltInType("uint16", 2);
    Type_uint32  = CreateBuiltInType("uint32", 4);
    Type_uint64  = CreateBuiltInType("uint64", 8);
    Type_float32 = CreateBuiltInType("float32", 4);
    Type_float64 = CreateBuiltInType("float64", 8);
    Type_char    = CreateBuiltInType("char", 1);
    Type_string  = CreateBuiltInType("string", 16);

    GlobalScope->Types["int"]     = Type_int64;
    GlobalScope->Types["uint"]    = Type_uint64;
    GlobalScope->Types["float"]   = Type_float32;

    TypeIdentifier_void = CreateTypeIdentifier(Type_void);
    TypeIdentifier_bool = CreateTypeIdentifier(Type_bool);
    TypeIdentifier_int8 = CreateTypeIdentifier(Type_int8);
    TypeIdentifier_int16 = CreateTypeIdentifier(Type_int16);
    TypeIdentifier_int32 = CreateTypeIdentifier(Type_int32);
    TypeIdentifier_int64 = CreateTypeIdentifier(Type_int64);
    TypeIdentifier_uint8 = CreateTypeIdentifier(Type_uint8);
    TypeIdentifier_uint16 = CreateTypeIdentifier(Type_uint16);
    TypeIdentifier_uint32 = CreateTypeIdentifier(Type_uint32);
    TypeIdentifier_uint64 = CreateTypeIdentifier(Type_uint64);
    TypeIdentifier_float32 = CreateTypeIdentifier(Type_float32);
    TypeIdentifier_float64 = CreateTypeIdentifier(Type_float64);
    TypeIdentifier_char = CreateTypeIdentifier(Type_char);
    TypeIdentifier_string = CreateTypeIdentifier(Type_string);
    TypePtr_char = CreateTypePointer(TypeIdentifier_char);

    TypeIdentifier_int = TypeIdentifier_int64;
    TypeIdentifier_uint = TypeIdentifier_uint64;

    Type_string->Members.push_back(CreateMemberVarDecl(Type_string, CreateVarDecl("begin", TypePtr_char), true));
    Type_string->Members.push_back(CreateMemberVarDecl(Type_string, CreateVarDecl("end", TypePtr_char), true));

    auto string_at = CreateNode<node_memberfuncdecl>();
    string_at->Declaration = CreateFuncDecl({}, "at");
    string_at->Declaration->ReturnType = TypeIdentifier_char;
    string_at->Declaration->Arguments.push_back(CreateVarDecl("", TypeIdentifier_uint));
    string_at->Declaration->IsInternal = true;
    Type_string->Members.push_back(string_at);

    auto string_count = CreateNode<node_memberfuncdecl>();
    string_count->Declaration = CreateFuncDecl({}, "count");
    string_count->Declaration->ReturnType = TypeIdentifier_uint;
    string_count->Declaration->IsInternal = true;
    Type_string->Members.push_back(string_count);

    BinaryOperators[ToInt(binop::Add)] = "+";
    BinaryOperators[ToInt(binop::Sub)] = "-";
    BinaryOperators[ToInt(binop::Mul)] = "*";
    BinaryOperators[ToInt(binop::Div)] = "/";
    BinaryOperators[ToInt(binop::Mod)] = "%";
    BinaryOperators[ToInt(binop::AddEq)] = "+=";
    BinaryOperators[ToInt(binop::SubEq)] = "-=";
    BinaryOperators[ToInt(binop::MulEq)] = "*=";
    BinaryOperators[ToInt(binop::DivEq)] = "/=";
    BinaryOperators[ToInt(binop::ModEq)] = "%=";
    BinaryOperators[ToInt(binop::Eq)] = "==";
    BinaryOperators[ToInt(binop::NEq)] = "!=";
    BinaryOperators[ToInt(binop::GT)] = ">";
    BinaryOperators[ToInt(binop::LT)] = "<";
    BinaryOperators[ToInt(binop::GET)] = ">=";
    BinaryOperators[ToInt(binop::LET)] = "<=";
    BinaryOperators[ToInt(binop::LAnd)] = "&&";
    BinaryOperators[ToInt(binop::LOr)] = "||";

    auto CreateBuiltinBinaryOp = [&](node_type* ReturnType, node_type* FirstType, node_type* SecondType) {
        auto Result = CreateFuncDecl(GlobalNode);
        auto FirstArg = CreateVarDecl(Result);
        auto SecondArg = CreateVarDecl(Result);
        FirstArg->Type = FirstType;
        SecondArg->Type = SecondType;
        Result->Arguments.push_back(FirstArg);
        Result->Arguments.push_back(SecondArg);
        Result->ReturnType = ReturnType;
        return Result;
    };
#if 0
    auto CreateBuiltinFunction = [&](string_view Identifier, node_type* ReturnType, list<node_type*> Args) {
        auto Result = CreateNode<node_functiondecl>();
        Result->Identifier = Identifier;
        Result->ReturnType = ReturnType;
        for (auto A : Args) {
            Result->Arguments.push_back(CreateVarDecl("", A));
        }
        return Result;
    };

    GlobalNode->Declarations.push_back(CreateBuiltinFunction("makeString", TypeIdentifier_string, {TypePtr_char, TypePtr_char}));
    GlobalNode->Declarations.push_back(CreateBuiltinFunction("makeString", TypeIdentifier_string, {TypePtr_char, TypeIdentifier_uint}));
#endif
    auto Binop_s8s8s8    = CreateBuiltinBinaryOp(TypeIdentifier_int8, TypeIdentifier_int8, TypeIdentifier_int8);
    auto Binop_s16s16s16 = CreateBuiltinBinaryOp(TypeIdentifier_int16, TypeIdentifier_int16, TypeIdentifier_int16);
    auto Binop_s32s32s32 = CreateBuiltinBinaryOp(TypeIdentifier_int32, TypeIdentifier_int32, TypeIdentifier_int32);
    auto Binop_s64s64s64 = CreateBuiltinBinaryOp(TypeIdentifier_int64, TypeIdentifier_int64, TypeIdentifier_int64);
    auto Binop_u8u8u8    = CreateBuiltinBinaryOp(TypeIdentifier_uint8, TypeIdentifier_uint8, TypeIdentifier_uint8);
    auto Binop_u16u16u16 = CreateBuiltinBinaryOp(TypeIdentifier_uint16, TypeIdentifier_uint16, TypeIdentifier_uint16);
    auto Binop_u32u32u32 = CreateBuiltinBinaryOp(TypeIdentifier_uint32, TypeIdentifier_uint32, TypeIdentifier_uint32);
    auto Binop_u64u64u64 = CreateBuiltinBinaryOp(TypeIdentifier_uint64, TypeIdentifier_uint64, TypeIdentifier_uint64);
    auto Binop_f32f32f32 = CreateBuiltinBinaryOp(TypeIdentifier_float32, TypeIdentifier_float32, TypeIdentifier_float32);
    auto Binop_f64f64f64 = CreateBuiltinBinaryOp(TypeIdentifier_float64, TypeIdentifier_float64, TypeIdentifier_float64);
    auto Binop_bs8s8   = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_int8, TypeIdentifier_int8);
    auto Binop_bs16s16 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_int16, TypeIdentifier_int16);
    auto Binop_bs32s32 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_int32, TypeIdentifier_int32);
    auto Binop_bs64s64 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_int64, TypeIdentifier_int64);
    auto Binop_bu8u8   = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_uint8, TypeIdentifier_uint8);
    auto Binop_bu16u16 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_uint16, TypeIdentifier_uint16);
    auto Binop_bu32u32 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_uint32, TypeIdentifier_uint32);
    auto Binop_bu64u64 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_uint64, TypeIdentifier_uint64);
    auto Binop_bf32f32 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_float32, TypeIdentifier_float32);
    auto Binop_bf64f64 = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_float64, TypeIdentifier_float64);
    auto Binop_bcc = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_char, TypeIdentifier_char);
    auto Binop_bss = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_string, TypeIdentifier_string);
    auto Binop_bbb = CreateBuiltinBinaryOp(TypeIdentifier_bool, TypeIdentifier_bool, TypeIdentifier_bool);
    GlobalNode->BinaryOperators[ToInt(binop::Add)] =
        GlobalNode->BinaryOperators[ToInt(binop::Sub)] =
        GlobalNode->BinaryOperators[ToInt(binop::Mul)] =
        GlobalNode->BinaryOperators[ToInt(binop::Div)] =
        GlobalNode->BinaryOperators[ToInt(binop::AddEq)] =
        GlobalNode->BinaryOperators[ToInt(binop::SubEq)] =
        GlobalNode->BinaryOperators[ToInt(binop::MulEq)] =
        GlobalNode->BinaryOperators[ToInt(binop::DivEq)] = {
            Binop_s8s8s8,
            Binop_s16s16s16,
            Binop_s32s32s32,
            Binop_s64s64s64,
            Binop_u8u8u8,
            Binop_u16u16u16,
            Binop_u32u32u32,
            Binop_u64u64u64,
            Binop_f32f32f32,
            Binop_f64f64f64
    };
    GlobalNode->BinaryOperators[ToInt(binop::Mod)] =
        GlobalNode->BinaryOperators[ToInt(binop::ModEq)] = {
            Binop_s8s8s8,
            Binop_s16s16s16,
            Binop_s32s32s32,
            Binop_s64s64s64,
            Binop_u8u8u8,
            Binop_u16u16u16,
            Binop_u32u32u32,
            Binop_u64u64u64,
    };
    GlobalNode->BinaryOperators[ToInt(binop::Eq)] =
        GlobalNode->BinaryOperators[ToInt(binop::NEq)] = {
            Binop_bs8s8,
            Binop_bs16s16,
            Binop_bs32s32,
            Binop_bs64s64,
            Binop_bu8u8,
            Binop_bu16u16,
            Binop_bu32u32,
            Binop_bu64u64,
            Binop_bf32f32,
            Binop_bf64f64,
            Binop_bcc,
            Binop_bss,
            Binop_bbb
    };
    GlobalNode->BinaryOperators[ToInt(binop::LT)] =
        GlobalNode->BinaryOperators[ToInt(binop::GT)] =
        GlobalNode->BinaryOperators[ToInt(binop::LET)] =
        GlobalNode->BinaryOperators[ToInt(binop::GET)] = {
            Binop_bs8s8,
            Binop_bs16s16,
            Binop_bs32s32,
            Binop_bs64s64,
            Binop_bu8u8,
            Binop_bu16u16,
            Binop_bu32u32,
            Binop_bu64u64,
            Binop_bf32f32,
            Binop_bf64f64,
    };
    GlobalNode->BinaryOperators[ToInt(binop::LOr)] =
        GlobalNode->BinaryOperators[ToInt(binop::LAnd)] = {
            Binop_bbb
    };

    auto CreateAsDecl = [&](node_type* Src, node_type* Dst) {
        auto Result = CreateNode<node_functiondecl>();
        Result->Arguments.push_back(CreateVarDecl({}, Src));
        Result->ReturnType = Dst;
        return Result;
    };

    GlobalNode->AsOperators = {
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_int8, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_int16, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_int64, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_uint8, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_uint16, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_uint32, TypeIdentifier_uint64),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_int8),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_int16),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_int32),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_int64),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_uint8),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_uint16),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_uint32),
        CreateAsDecl(TypeIdentifier_uint64, TypeIdentifier_uint64),

        // HACK:
        // CreateAsDecl(TypeIdentifier_int32, TypeIdentifier_uint32),
    };

    auto CreateNegateOperator = [&](node_type* Src, node_type* Dst) {
        auto Result = CreateNode<node_functiondecl>();
        Result->Arguments.push_back(CreateVarDecl({}, Src));
        Result->ReturnType = Dst;
        return Result;
    };

    GlobalNode->NegateOperators = {
        CreateNegateOperator(TypeIdentifier_int8, TypeIdentifier_int8),
        CreateNegateOperator(TypeIdentifier_int16, TypeIdentifier_int16),
        CreateNegateOperator(TypeIdentifier_int32, TypeIdentifier_int32),
        CreateNegateOperator(TypeIdentifier_int64, TypeIdentifier_int64),
    };

    Success = true;

    ImportDirectories.push_back(ToString(Compiler->CompilerDirectory) + DIR_LITERAL("imports\\"));

    if (!ParseMainLoop()) {
        Success = false;
        return;
    }
    bool HasEntryPoint = false;
    for (auto Node : GlobalNode->Declarations) {
        if (Node->AstType == NodeType_FunctionDecl && ((node_functiondecl*)Node)->Identifier == "main") {
            HasEntryPoint = true;
            break;
        }
    }
    if (!HasEntryPoint) {
        puts("Entry point is not defined. To build an app you must define a function called 'main' which takes no arguments and returns 'int'");
        Success = false;
        return;
    }
    if (LinkNodes() != LINK_OK) {
        Success = false;
    }

    for (auto ptr : DeleteAfterLink)
        delete ptr;

    if (CustomBinDir.Count()) {
        BinDir = ToString(CustomBinDir);
        for (auto& C : BinDir) {
            if (C == '/')
                C = '\\';
        }
        if (BinDir.back() != '\\') {
            BinDir.push_back('\\');
        }
    }
    if (CustomTempDir.Count()) {
        TempDir = ToString(CustomTempDir);
        for (auto& C : TempDir) {
            if (C == '/')
                C = '\\';
        }
        if (TempDir.back() != '\\') {
            TempDir.push_back('\\');
        }
    }
}

#define ENSURE_NEXT_TOKEN(prsr, ret, ...) if(!(prsr).NextToken()) { ReportError((prsr).TOKEN.Location, "Unexpected end of file"); __VA_ARGS__; return ret; }

int parser::ParseIteration() {
    if (TOKEN.Value == "import") {
        ENSURE_NEXT_TOKEN(*this, PARSE_FAIL_FINISH);
        if (!ExpectStringLiteral()) {
            return PARSE_FAIL;
        }

        auto ImportFile = FindImportFile(TOKEN.Value);
        if (!ImportFile.File) {
            ReportError(TOKEN.Location, "Can't find import file '", TOKEN.Value, "'");
            return PARSE_FAIL;
        }
        if (std::find(compiler::Instance->CompilingFilePaths.begin(), compiler::Instance->CompilingFilePaths.end(), ToString(ImportFile.FullPath)) != compiler::Instance->CompilingFilePaths.end()) {
            NEXT_TOKEN_OK_FINISH;
        }
        compiler::Instance->CompilingFilePaths.push_back(ToString(ImportFile.FullPath));
        auto FileContents= compiler::Instance->LoadFileContents(ImportFile.File);

        bool Ok = true;

        lexer* OldLexer = Lexer;
        Lexer = new lexer;
        Lexer->Process(FileContents);
        if (Lexer->Success) {
            Ok = ParseImportLoop();
        }
        else {
            Ok = false;
        }

        delete Lexer;
        Lexer = OldLexer;
        fclose(ImportFile.File);

        if (!Ok)
            return PARSE_FAIL;

        NEXT_TOKEN_OK_FINISH;
    }
    if (TOKEN.Value == "operator") {
        auto Location = TOKEN.Location;
        ENSURE_NEXT_TOKEN(*this, PARSE_FAIL_FINISH);
        if (!IsBinaryOperator(TOKEN.Value)) {
            ReportError(TOKEN.Location, "'", TOKEN.Value, "' is not valid binary operator");
            return PARSE_FAIL;
        }
        auto OpStr = TOKEN.Value;
        auto Decl = ParseFunDeclArgsAndBody(Location, OpStr);
        if (!Decl) {
            return PARSE_FAIL;
        }
        auto Op = ToInt(BinopFromString(OpStr));
        GlobalNode->BinaryOperators[Op].push_back(Decl);
        NEXT_TOKEN_OK_FINISH;
    }
    auto Beginning = TOKEN.Location;
    auto Decl = ParseDeclaration();
    if (Decl) {
        GlobalNode->Declarations.push_back(Decl);
    }
    switch (ParseCode) {
        case PARSE_OTHER:
            ReportError(Beginning, "It's not a declaration. Only declarations can appear in global scope");
            return PARSE_FAIL;
        default:
            return ParseCode;
    }
}
bool parser::ParseMainLoop() {
    struct build_param {
        bool (*Evaluate)(parser&) = 0;
    };
    std::unordered_map<string_view, build_param> BuildParams;
    BuildParams["type"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        if (Parser.TOKEN.Value == "app") {
            Parser.BuildType = build_type::App;
        }
        else if (Parser.TOKEN.Value == "slib") {
            Parser.BuildType = build_type::SLib;
        }
        else if (Parser.TOKEN.Value == "dlib") {
            Parser.BuildType = build_type::DLib;
        }
        else {
            ReportError(Parser.TOKEN.Location, "Expected 'app', 'slib' or 'dlib', got '", Parser.TOKEN.Value, "'");
            return false;
        }
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    BuildParams["out"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        Parser.OutFile = Parser.TOKEN.Value;
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    BuildParams["translate"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        Parser.TranslatorName = Parser.TOKEN.Value;
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    BuildParams["arch"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        Parser.TargetArch = CreateArch(Parser.TOKEN.Value, Parser.TOKEN.Location);
        if (Parser.TargetArch.Type == arch_type::Null)
            return false;
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    BuildParams["bindir"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        Parser.CustomBinDir = Parser.TOKEN.Value;
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    BuildParams["tempdir"].Evaluate = [](parser& Parser) {
        if (!Parser.ExpectStringLiteral()) { return false; }
        Parser.CustomTempDir = Parser.TOKEN.Value;
        ENSURE_NEXT_TOKEN(Parser, false);
        return true;
    };
    while (true) {
        if (TOKEN.Value == "build") {
            ENSURE_NEXT_TOKEN(*this, false);
            if (!Expect("{")) {
                return false;
            }
            ENSURE_NEXT_TOKEN(*this, false);
            while (TOKEN.Value != "}") {
                if (!ExpectIdentifier()) {
                    return false;
                }
                auto BuildIter = BuildParams.find(TOKEN.Value);
                if (BuildIter != BuildParams.end()) {
                    ENSURE_NEXT_TOKEN(*this, false);
                    if (!BuildIter->second.Evaluate(*this)) {
                        return false;
                    }
                }
                else {
                    string_builder Builder;
                    Builder.Append("Expected ");
                    for (auto& p : BuildParams) {
                        Builder.Append("'");
                        Builder.Append(p.first);
                        Builder.Append("', ");
                    }
                    Builder.Append("got '");
                    Builder.Append(TOKEN.Value);
                    Builder.Append("'");
                    ReportError(TOKEN.Location, Builder.GetString());
                    return false;
                }
            }
            ENSURE_NEXT_TOKEN(*this, false);
            continue;
        }
        switch (ParseIteration()) {
            case PARSE_OK: continue;
            case PARSE_OK_FINISH: return true;
            case PARSE_FAIL_FINISH:
            case PARSE_FAIL: return false;
            default:
                assert(0);
        }
    }
}
bool parser::ParseImportLoop() {
    while (true) {
        switch (ParseIteration()) {
            case PARSE_OK: continue;
            case PARSE_OK_FINISH: return true;
            case PARSE_FAIL_FINISH:
            case PARSE_FAIL: return false;
            default:
                assert(0);
        }
    }
}

#define NEW_SCOPE                                               \
scope* OldScope = CurrentScope;                                 \
CurrentScope = CurrentScope->Children.emplace_back(new scope);  \
CurrentScope->Parent = OldScope;                                \
defer _([&]() {CurrentScope = OldScope; });

node_type* parser::ParseType() {
    auto StartTokenIndex = Lexer->TokenIndex;
    auto RestoreToken = [&]() {
        TOKEN = Lexer->Tokens[Lexer->TokenIndex = StartTokenIndex];
    };
    node_type* Result = 0;
    if (TOKEN.Value == TOKEN_SUBSRCIPT_BEGIN) {
        auto Array = CreateTypeArray(TOKEN.Location);
        Result = Array;
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        if (TOKEN.Type != TokenType_Literal && TOKEN.DataType != data_type::UInt) {
            RestoreToken();
            return 0;
        }
        Array->Count = ToU32_Unchecked(TOKEN.Value);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        if (TOKEN.Value != TOKEN_SUBSRCIPT_END) {
            RestoreToken();
            return 0;
        }
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Array->Next = ParseType();
        if (!Array->Next) {
            RestoreToken();
            return 0;
        }
    }
    else if (TOKEN.Value == TOKEN_POINTER) {
        auto Pointer = CreateTypePointer(TOKEN.Location);
        Result = Pointer;
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Pointer->Next = ParseType();
        if (!Pointer->Next) {
            RestoreToken();
            return 0;
        }
    }
    else if (TOKEN.Type == TokenType_Identifier) {
        if (TOKEN.Value == "auto") {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            return TYPE_AUTO;
        }
        Result = CreateTypeIdentifier(TOKEN);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
    }
    else {
        RestoreToken();
        return 0;
    }
    return Result;
}
node_vardecl* parser::ParseVarDeclInitialExpression(token Identifier, node_type* Type) {
    int Result = PARSE_OK;
    auto Declaration = CreateVarDecl(Identifier.Location, Identifier.Value, Type, 0);
    if (TOKEN.Value == "=") {
        if (!NextToken()) {
            ReportError(TOKEN.Location, "Expected an expression after '=', not end of file");
            ParseCode = PARSE_FAIL;
            return 0;
        }
        Declaration->InitialExpression = ParseExpression();
    }
    auto Redecl = TryFindVarDecl(Declaration, Identifier.Value);
    if (Redecl) {
        ReportError(Identifier.Location, "'", Identifier.Value, "' is already declared in ", Redecl->Location);
        ParseCode = PARSE_FAIL;
        return 0;
    }
    ParseCode = Result;
    return Declaration;
}
node_vardecl* parser::ParseVarDecl() {
    auto StartTokenIndex = Lexer->TokenIndex;
    auto RestoreToken = [&]() {
        TOKEN = Lexer->Tokens[Lexer->TokenIndex = StartTokenIndex];
    };

    if (TOKEN.Type != TokenType_Identifier) {
        ParseCode = PARSE_OTHER;
        return 0;
    }
    token Identifier = TOKEN;
    if (!IsIdentifierValid(Identifier.Value)) {
        ReportError(Identifier.Location, "'", Identifier.Value, "' is not a valid identifier for a variable");
        ParseCode = PARSE_FAIL;
        return 0;
    }
    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
    node_type* Type = ParseType();
    if (!Type) {
        RestoreToken();
        ParseCode = PARSE_OTHER;
        return 0;
    }
    return ParseVarDeclInitialExpression(Identifier, Type);
}
node_expression* parser::ParseExpression() {
    node_expression* Result = ParseExpressionNoBinop();
    if (!Result)
        return 0;
    while (1) {
        if (IsBinaryOperator(TOKEN.Value)) {
            node_binaryop* BinaryOp = CreateBinaryOp(TOKEN.Location, BinopFromString(TOKEN.Value));
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            BinaryOp->FirstExpression = Result;
            Result->Parent = BinaryOp;
            if (GetBinopType(BinaryOp->Op) == binop_type::Logic)
                BinaryOp->SecondExpression = ParseExpression();
            else
                BinaryOp->SecondExpression = ParseExpressionNoBinop();
            if (!BinaryOp->SecondExpression)
                return 0;
            if ((BinaryOp->FirstExpression->Flags & AstFlag_Known) && (BinaryOp->SecondExpression->Flags & AstFlag_Known)) {
                BinaryOp->Flags |= AstFlag_Known;
            }
            Result = BinaryOp;
        }
        else break;
        continue;
    }
    return Result;
}
node_expression* parser::ParseExpressionNoBinopNoAs() {
    node_expression* Result = 0;
    if (TOKEN.Type == TokenType_Literal) {
        Result = CreateLiteral(TOKEN);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
    }
    else {
        if (TOKEN.Value == TOKEN_BRACEINIT_BEGIN) {
            node_braceinit* Braceinit = CreateNode<node_braceinit>(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            if (TOKEN.Value != TOKEN_BRACEINIT_END) {
                ReportError(Braceinit->Location, "Unclosed brace init");
                return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Result = Braceinit;
        }
        else if (TOKEN.Value == TOKEN_PAREN_BEGIN) {
            node_paren* Paren = CreateParen(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Paren->Expression = ParseExpression();
            if (!Paren->Expression)
                return 0;
            if (TOKEN.Value != TOKEN_PAREN_END) {
                ReportError(Paren->Location, "Unclosed paren");
                return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Paren->Flags |= Paren->Expression->Flags & AstFlag_Known;
            Result = Paren;
        }
        else if (TOKEN.Value == TOKEN_ADDRESS) {
            node_address* Address = CreateNode<node_address>(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Address->Expression = ParseExpressionNoBinop();
            Result = Address;
        }
        else if (TOKEN.Value == TOKEN_DEREF) {
            node_deref* Deref = CreateNode<node_deref>(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Deref->Expression = ParseExpressionNoBinop();
            Result = Deref;
        }
        else if (TOKEN.Value == TOKEN_SUB) {
            node_negate* Negate = CreateNode<node_negate>(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Negate->Expression = ParseExpressionNoBinop();
            Negate->Flags |= Negate->Expression->Flags & AstFlag_Known;
            Result = Negate;
        }
        else if (TOKEN.Value == TOKEN_NOT) {
            node_not* Not = CreateNode<node_not>(TOKEN.Location);
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Not->Expression = ParseExpressionNoBinop();
            Not->Flags |= Not->Expression->Flags & AstFlag_Known;
            Result = Not;
        }
        else if (TOKEN.Value == "sizeof") {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            auto Sizeof = CreateSizeof(TOKEN.Location, ParseType());
            if (!EnsureValid(Sizeof->TestType))
                return 0;
            Sizeof->Type = TypeIdentifier_uint;
            Result = Sizeof;
        }
        else if (PeekToken().Value == TOKEN_ARGLIST_BEGIN) {
            Result = ParseFunctionCall();
        }
        else {
            if (TOKEN.Type == TokenType_Identifier) {
                Result = CreateVarAccess(TOKEN.Location, TOKEN.Value);
                ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            }
            else {
                ReportError(TOKEN.Location, "???");
                return 0;
            }
        }
    }
    if (TOKEN.Value == TOKEN_SUBSRCIPT_BEGIN) {
        auto Index = CreateNode<node_index>(TOKEN.Location);
        Index->Identifier = TOKEN.Value;
        Index->Expression = Result;
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Index->IndexExpression = ParseExpression();
        Expect(TOKEN_SUBSRCIPT_END);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Result->Parent = Index;
        Result = Index;
    }
    if (TOKEN.Value == TOKEN_MEMBER) {
        auto Expr = CreateMemberAccess(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Result->Parent = Expr;
        Expr->First = Result;
        Expr->Second = ParseExpressionNoBinopNoAs();
        if (!Expr->Second)
            return 0;
        Result = Expr;
    }
    return Result;
}
node_expression* parser::ParseExpressionNoBinop() {
    node_expression* Result = ParseExpressionNoBinopNoAs();
    if (TOKEN.Value == "as") {
        auto As = CreateNode<node_as>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Result->Parent = As;
        As->Expression = Result;
        As->Type = ParseType();
        if (!As->Type)
            return 0;
        Result = As;
    }
    return Result;
}
node_functioncall* parser::ParseFunctionCall() {
    token Identifier = TOKEN;
    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
    if (TOKEN.Value == TOKEN_ARGLIST_BEGIN) {
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        node_functioncall* Call = CreateFuncCall(Identifier.Location, Identifier.Value);
        if (TOKEN.Value != TOKEN_ARGLIST_END) {
            while (1) {
                node_expression* Arg = ParseExpression();
                if (!Arg) {
                    ReportError(TOKEN.Location, "Failed to parse function argument");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
                Call->Arguments.push_back(Arg);
                if (TOKEN.Value == ",") {
                    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                    continue;
                }
                else if (TOKEN.Value == TOKEN_ARGLIST_END) {
                    break;
                }
                else {
                    ReportError(TOKEN.Location, "Only ',' or ')' can appear after function argument");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
            }
        }


        if (NextToken())
            ParseCode = PARSE_OK;
        else
            ParseCode = PARSE_OK_FINISH;
        return Call;
    }
    PrevToken();
    return 0;
}
node_functiondecl* parser::ParseFunDeclArgsAndBody(location Location, string_view Identifier) {
    NEW_SCOPE;

    node_functiondecl* Declaration = CreateFuncDecl(Location, Identifier);
    Declaration->Scope = CurrentScope;

#if 0
    if (TOKEN.Value == TOKEN_TYPELIST_BEGIN) {
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        while (true) {
            auto Identifier = TOKEN.Value;
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            Expect("type");
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            CurrentScope->Types[Identifier] = 0;
            Declaration->TypeArguments.push_back(Identifier);
            if (TOKEN.Value == ",") {
                ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                continue;
            }
            else if (TOKEN.Value == TOKEN_TYPELIST_END) {
                ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                break;
            }
            else {
                ReportError(TOKEN.Location, "Only ',' or '" TOKEN_TYPELIST_END "' can appear after function type parameter declaration");
                ParseCode = PARSE_FAIL;
                return 0;
            }
        }
    }
#endif
    if (TOKEN.Value == TOKEN_ARGLIST_BEGIN) {
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        if (TOKEN.Value == TOKEN_ARGLIST_END) {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        }
        else {
            while (true) {
                auto Arg = ParseVarDecl();
                if (ParseCode == PARSE_OK) {
                    if (Arg->Type == TYPE_AUTO) {
                        ReportError(Arg->Location, "'auto' is not allowed in function arguments");
                        ParseCode = PARSE_FAIL;
                        return 0;
                    }
                    else {
                        Declaration->Arguments.push_back(Arg);
                    }
                }
                else {
                    ReportError(TOKEN.Location, "Failed to parse function argument");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
                if (TOKEN.Value == ",") {
                    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                    continue;
                }
                else if (TOKEN.Value == TOKEN_ARGLIST_END) {
                    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                    break;
                }
                else {
                    ReportError(TOKEN.Location, "Only ',' or '" TOKEN_ARGLIST_END "' can appear after function parameter declaration");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
            }
        }
    }
    if (TOKEN.Value == TOKEN_RETTYPE_DELIM) {
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Declaration->ReturnType = ParseType();
        if (!EnsureValid(Declaration->ReturnType)) {
            ReportError(TOKEN.Location, "Failed to parse return type");
            ParseCode = PARSE_FAIL;
            return 0;
        }
    }
    else {
        Declaration->ReturnType = CreateTypeIdentifier({}, Type_void);
    }

    if (TOKEN.Value == "#internal") {
        Declaration->IsInternal = true;
    }
    else {
        if (!Expect(TOKEN_BODY_BEGIN)) {
            ParseCode = PARSE_FAIL;
            return 0;
        }
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);

        while (TOKEN.Value != TOKEN_BODY_END) {
            auto Stm = ParseStatement(Declaration->Statements);
            if (Stm) {
                Declaration->Statements.push_back(Stm);
            }
            else {
                switch (ParseCode) {
                    case PARSE_FAIL_FINISH:
                    case PARSE_OK_FINISH:
                        ReportError(Declaration->Location, "Function body not closed");
                        break;
                    case PARSE_FAIL:
                        ReportError(TOKEN.Location, "Unresolved statement");
                        break;
                    default:
                        assert(0);
                }
                ParseCode = PARSE_FAIL;
                return 0;
            }
        }

    }
    //GetChildren(Parent)->push_back(Declaration);
    if (NextToken())
        ParseCode = PARSE_OK;
    else
        ParseCode = PARSE_OK_FINISH;

    return Declaration;
}
node* parser::ParseDeclaration() {
    token Identifier = TOKEN;
    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
    if (TOKEN.Type == TokenType_Identifier) {
        if (TOKEN.Value == "function") {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            return ParseFunDeclArgsAndBody(Identifier.Location, Identifier.Value);
        }
        else if (TOKEN.Value == "type") {
            auto Redecl = TryFindTypeDecl(Identifier.Value);
            if (Redecl) {
                ReportError(Identifier.Location, "'", Identifier.Value, "' is already declared in ", Redecl->Location);
                ParseCode = PARSE_FAIL;
                return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            if (TOKEN.Value == "=") {
                ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                token OldIdentifier = TOKEN;

                node_typealias* Declaration = CreateTypeAlias(Identifier.Location, OldIdentifier.Value, Identifier.Value);
                //switch (Parent->AstType) {
                //    case NodeType_Global: ((node_global*)Parent)->Declarations.push_back(Declaration); break;
                //    case NodeType_FunctionDecl: ((node_functiondecl*)Parent)->Statements.push_back(Declaration); break;
                //    default:
                //        assert(0);
                //        break;
                //}

                if (NextToken())
                    ParseCode = PARSE_OK;
                else
                    ParseCode = PARSE_OK_FINISH;
                return Declaration;
            }
            else {
                node_typedecl* TypeDecl = CreateTypeDecl(Identifier.Location, Identifier.Value);
                while (TOKEN.Value != TOKEN_BODY_BEGIN) {
                    if (TOKEN.Value == "#align") {
                        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                        TypeDecl->Align.Expression = ParseExpression();
                        if (!(TypeDecl->Align.Expression->Flags & AstFlag_Known)) {
                            ReportError(TypeDecl->Align.Expression->Location, "Expression must be known at parse time");
                            return 0;
                        }
                        TypeDecl->Align.Custom = true;
                        TypeDecl->Align.Value = EvaluateInt(TypeDecl->Align.Expression);
                    }
                    else if (TOKEN.Value == "#pack") {
                        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                        TypeDecl->Packing.Expression = ParseExpression();
                        if (!(TypeDecl->Packing.Expression->Flags & AstFlag_Known)) {
                            ReportError(TypeDecl->Packing.Expression->Location, "Expression must be known at parse time");
                            return 0;
                        }
                        TypeDecl->Packing.Custom = true;
                        TypeDecl->Packing.Value = EvaluateInt(TypeDecl->Packing.Expression);
                    }
                    else {
                        ReportError(TOKEN.Location, "Unexpected '", TOKEN.Value, "'");
                        ParseCode = PARSE_FAIL;
                        return 0;
                    }
                }
                ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                while (TOKEN.Value != TOKEN_BODY_END) {
                    auto Decl = ParseDeclaration();
                    if (Decl) {
                        switch (Decl->AstType) {
                            case NodeType_VarDecl: {
                                auto Member = CreateMemberVarDecl(TOKEN.Location, (node_vardecl*)Decl);
                                TypeDecl->Members.push_back(Member);
                                if (Member->Declaration->Type == TYPE_AUTO) {
                                    ReportError(Member->Location, "'auto' is not allowed in types");
                                    ParseCode = PARSE_FAIL;
                                    return 0;
                                }
                                if (TOKEN.Value == "#offset") {
                                    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                                    Member->Offset.Expression = ParseExpression();
                                    if (!(Member->Offset.Expression->Flags & AstFlag_Known)) {
                                        ReportError(Member->Offset.Expression->Location, "Expression must be known at parse time");
                                        return 0;
                                    }
                                    Member->Offset.Custom = true;
                                    Member->Offset.Value = EvaluateInt(Member->Offset.Expression);
                                }
                                break;
                            }
                            case NodeType_FunctionDecl: {
                                auto Member = CreateMemberFuncDecl(TOKEN.Location, (node_functiondecl*)Decl);
                                TypeDecl->Members.push_back(Member);
                                break;
                            }
                            case NodeType_TypeDecl:
                                TypeDecl->Members.push_back(Decl);
                                break;
                            default:
                                assert(0);
                                break;
                        }
                    }
                    switch (ParseCode) {
                        case PARSE_OTHER: {
                            ReportError(TOKEN.Location, "It's not a declaration");
                            ParseCode = PARSE_FAIL;
                            return 0;
                        }
                        case PARSE_FAIL_FINISH:
                        case PARSE_OK_FINISH: {
                            ReportError(TOKEN.Location, "Typedecl body not closed");
                            ParseCode = PARSE_FAIL;
                            return 0;
                        }
                        case PARSE_FAIL: {
                            ReportError(TOKEN.Location, "Failed to parse declaration");
                            ParseCode = PARSE_FAIL;
                            return 0;
                        }
                    }
                }
                //GetChildren(Parent)->push_back(TypeDecl);
                CurrentScope->Types[Identifier.Value] = TypeDecl;
                if (NextToken())
                    ParseCode = PARSE_OK;
                else
                    ParseCode = PARSE_OK_FINISH;
                return TypeDecl;
            }
        }
        else if (TOKEN.Value == "namespace") {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            if (!Expect(TOKEN_BODY_BEGIN)) {
                ParseCode = PARSE_FAIL;
                return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            node_nsdecl* Namespace = CreateNSDecl(Identifier.Location, Identifier.Value);
            while (TOKEN.Value != TOKEN_BODY_END) {
                auto Decl = ParseDeclaration();
                if (Decl) {
                    Namespace->Declarations.push_back(Decl);
                }
                else {
                    ReportError(TOKEN.Location, "Failed to parse declaration");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
            }
            //GetChildren(Parent)->push_back(Namespace);
            if (NextToken())
                ParseCode = PARSE_OK;
            else
                ParseCode = PARSE_OK_FINISH;
            return Namespace;
        }
        else {
            PrevToken();
            auto VarDecl = ParseVarDecl();
            if (VarDecl)
                ParseCode = PARSE_OK;
            else
                ParseCode = PARSE_OTHER;
            return VarDecl;
        }
    }
    else {
        auto Type = ParseType();
        if (Type) {
            auto VarDecl = ParseVarDeclInitialExpression(Identifier, Type);
            if (VarDecl)
                ParseCode = PARSE_OK;
            else
                ParseCode = PARSE_FAIL;
            return VarDecl;
        }
        else {
            PrevToken();
            ParseCode = PARSE_OTHER;
            return 0;
        }
    }
}
node* parser::ParseStatement(const list<node*>& ParentStatements) {
    auto ParseStm = [&](list<node*>& Statements) {
        auto Stm = ParseStatement(Statements);
        if (Stm) {
            Statements.push_back(Stm);
            return true;
        }
        else {
            ReportError(TOKEN.Location, "Failed to parse statement");
            ParseCode = PARSE_FAIL;
            return false;
        }
    };
    if (TOKEN.Value == "return") {
        node_return* Return = CreateReturn(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        Return->Expression = ParseExpression();
        if (!Return->Expression) {
            ReportError(TOKEN.Location, "Failed to parse expression");
            ParseCode = PARSE_FAIL;
            return 0;
        }
        ParseCode = PARSE_OK;
        return Return;
    }
    else if (TOKEN.Value == "break") {
        node_break* Break = CreateNode<node_break>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        ParseCode = PARSE_OK;
        return Break;
    }
    else if (TOKEN.Value == "continue") {
        node_continue* Continue = CreateNode<node_continue>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        ParseCode = PARSE_OK;
        return Continue;
    }
    else if (TOKEN.Value == "if") {
        NEW_SCOPE;
        node_if* If = CreateNode<node_if>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        If->Condition = ParseExpression();
        if (TOKEN.Value == TOKEN_BODY_BEGIN) {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            while (TOKEN.Value != TOKEN_BODY_END) {
                if (!ParseStm(If->Statements))
                    return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        }
        else {
            if (!ParseStm(If->Statements))
                return 0;
        }
        ParseCode = PARSE_OK;
        return If;
    }
    else if (TOKEN.Value == "else") {
        NEW_SCOPE;
        if (ParentStatements.back()->AstType != NodeType_If) {
            ReportError(TOKEN.Location, "'else' can appear only after if statemetnt");
            ParseCode = PARSE_FAIL;
            return 0;
        }
        node_else* Else = CreateNode<node_else>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        if (TOKEN.Value == TOKEN_BODY_BEGIN) {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            while (TOKEN.Value != TOKEN_BODY_END) {
                if (!ParseStm(Else->Statements))
                    return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        }
        else {
            if (!ParseStm(Else->Statements))
                return 0;
        }
        ParseCode = PARSE_OK;
        return Else;
    }
    else if (TOKEN.Value == TOKEN_DEFER) {
        NEW_SCOPE;
        node_defer* Defer = CreateNode<node_defer>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        if (TOKEN.Value == TOKEN_BODY_BEGIN) {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            while (TOKEN.Value != TOKEN_BODY_END) {
                if (!ParseStm(Defer->Statements))
                    return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        }
        else {
            if (!ParseStm(Defer->Statements))
                return 0;
        }
        ParseCode = PARSE_OK;
        return Defer;
    }
    else if (TOKEN.Value == "while") {
        NEW_SCOPE;
        node_while* While = CreateNode<node_while>(TOKEN.Location);
        ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        While->Condition = ParseExpression();
        if (TOKEN.Value == TOKEN_BODY_BEGIN) {
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
            while (TOKEN.Value != TOKEN_BODY_END) {
                if (!ParseStm(While->Statements))
                    return 0;
            }
            ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
        }
        else {
            if (!ParseStm(While->Statements))
                return 0;
        }
        ParseCode = PARSE_OK;
        return While;
    }
    else {
        auto Decl = ParseDeclaration();
        if (Decl) {
            ParseCode = PARSE_OK;
            return Decl;
        }
        else {
            if (ParseCode == PARSE_OTHER) {
                node_expression* Expression = ParseExpression();
                if (!Expression) {
                    ReportError(TOKEN.Location, "Failed to parse expression");
                    ParseCode = PARSE_FAIL;
                    return 0;
                }
                if (TOKEN.Value == "=") {
                    node_assignment* Assignment = CreateAssignment(TOKEN.Location);
                    ENSURE_NEXT_TOKEN(*this, 0, ParseCode = PARSE_FAIL_FINISH);
                    Assignment->Assigned = Expression;
                    Assignment->Expression = ParseExpression();
                    if (!Assignment->Expression) {
                        ReportError(TOKEN.Location, "Failed to parse expression");
                        ParseCode = PARSE_FAIL;
                        return 0;
                    }
                    ParseCode = PARSE_OK;
                    return Assignment;
                }
                else {
                    if (IsStatement(Expression)) {
                        ParseCode = PARSE_OK;
                        return Expression;
                    }
                    else {
                        ReportError(Expression->Location, "This expression is not a statement");
                        ParseCode = PARSE_FAIL;
                        return 0;
                    }
                }
            }
            else {
                return 0;
            }
        }
    }
}

void SetDecl(node_type* Type, node_typedecl* TypeDecl) {
    // TODO loop
    while (Type) {
        switch (Type->Type) {
            case type::Array: Type = ((node_type_array*)Type)->Next; break;
            case type::Pointer: Type = ((node_type_pointer*)Type)->Next; break;
            case type::Identifier: ((node_type_identifier*)Type)->Decl = TypeDecl; break;
            default:
                assert(0);
                return;
        }
    }
}

node_type_identifier* GetTypeIdentifier(node_type* Type) {
    // TODO loop
    switch (Type->Type) {
        case type::Array: return GetTypeIdentifier(((node_type_array*)Type)->Next);
        case type::Pointer: return GetTypeIdentifier(((node_type_pointer*)Type)->Next);
        case type::Identifier: return (node_type_identifier*)Type;
    }
    assert(0);
    return 0;
}
#define LINK_INIT_NODE(name)          \
{                                     \
    assert(name->Parent);             \
    if (name->Flags & AstFlag_Linked) \
        return LINK_OK;               \
    name->Flags |= AstFlag_Linked;    \
}
#define SET_PARENT(parent, child) parent->child->Parent = parent

void SetFlag(u32& Dst, u32 Src, u32 Bit) {
    Dst &= ~Bit;
    Dst |= Src & Bit;
}

int GetPriority(node* Node) {
    switch (Node->AstType) {
        case NodeType_TypeAlias: return 0;
        case NodeType_TypeDecl: return 1;
        case NodeType_NSDecl: return 2;
        case NodeType_VarDecl: return 3;
        case NodeType_FunctionDecl: return 4;
        default:
            assert(0);
            return 0;
    }
}
int parser::LinkNodes() {
    std::sort(GlobalNode->Declarations.begin(), GlobalNode->Declarations.end(), [](node* A, node* B) {
        return GetPriority(A) < GetPriority(B);
              });
    for (node* Node : GlobalNode->Declarations) {
        Node->Parent = GlobalNode;
        if (LinkNode(Node) != LINK_OK)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_assignment* Assignment) {
    LINK_INIT_NODE(Assignment);
    SET_PARENT(Assignment, Assigned);
    SET_PARENT(Assignment, Expression);
    if (LinkNode(Assignment->Assigned) != LINK_OK) {
        return LINK_FATAL;
    }
    if (LinkNode(Assignment->Expression) != LINK_OK) {
        return LINK_FATAL;
    }
    // NOTE: Variables can be declared after usage! so make sure they are linked
    if (Assignment->Assigned->AstType == NodeType_Identifier && LinkNode(GetDeclaration(Assignment->Assigned)) != LINK_OK) {
        return LINK_FATAL;
    }
    if (Assignment->Expression->AstType == NodeType_Identifier && LinkNode(GetDeclaration(Assignment->Expression)) != LINK_OK) {
        return LINK_FATAL;
    }

    auto AssignedType = Assignment->Assigned->Type;
    auto ExpressionType = Assignment->Expression->Type;

    assert(AssignedType);
    assert(ExpressionType);
    assert(GetTypeDecl(AssignedType));
    assert(GetTypeDecl(ExpressionType));

    if (!EnsureImplicitlyConvertible(Assignment->Expression, Assignment->Assigned, Assignment->Location)) {
        return LINK_ERROR;
    }
    return LINK_OK;
}
int parser::LinkNode(node_vardecl* VarDecl) {
    LINK_INIT_NODE(VarDecl);
    if (VarDecl->InitialExpression)
        SET_PARENT(VarDecl, InitialExpression);
    if (VarDecl->Type == TYPE_AUTO) {
        if (VarDecl->InitialExpression) {
            if (LinkNode(VarDecl->InitialExpression) != LINK_OK) {
                return LINK_FATAL;
            }
            VarDecl->Type = VarDecl->InitialExpression->Type;
        }
        else {
            ReportError(VarDecl->Location, "Variable declared with 'auto' keyword must have an initializer");
            return LINK_FATAL;
        }
    }
    else {
        node_type_identifier* TypeIdentifier = GetTypeIdentifier(VarDecl->Type);
        TypeIdentifier->Decl = FindTypeDecl(VarDecl->Type->Location, TypeIdentifier->Identifier);
        if (!TypeIdentifier->Decl) {
            ReportError(VarDecl->Location, "Failed to declare variable '", VarDecl->Identifier, "'");
            return LINK_FATAL;
        }
        if (VarDecl->InitialExpression) {
            if (LinkNode(VarDecl->InitialExpression) != LINK_OK) {
                return LINK_FATAL;
            }
            if (!EnsureImplicitlyConvertible(VarDecl->InitialExpression, VarDecl->Type, VarDecl->Location)) {
                return LINK_ERROR;
            }
        }
    }
    if (Equals(VarDecl->Type, TypeIdentifier_void)) {
        ReportError(VarDecl->Location, "Variable can't be of type 'void'");
        return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkHeader(node_functiondecl* FunctionDecl) {
    node_type_identifier* Type = GetTypeIdentifier(FunctionDecl->ReturnType);
    if (!Type->Decl) {
        Type->Decl = FindTypeDecl(FunctionDecl->Location, Type->Identifier);
        if (!Type->Decl)
            return LINK_FATAL;
    }
    for (node_vardecl* Arg : FunctionDecl->Arguments) {
        Arg->Parent = FunctionDecl;
        Type = GetTypeIdentifier(Arg->Type);
        Type->Decl = FindTypeDecl(Arg->Location, Type->Identifier);
        if (!Type->Decl)
            return LINK_FATAL;
        if (Arg->InitialExpression) {
            Arg->InitialExpression->Parent = Arg;
            if (LinkNode(Arg->InitialExpression) != LINK_OK) {
                return LINK_FATAL;
            }
            if (!EnsureImplicitlyConvertible(Arg->InitialExpression, Arg->Type, Arg->Location)) {
                return LINK_FATAL;
            }
        }
    }
    return LINK_OK;
}
int parser::LinkNode(node_functiondecl* FunctionDecl) {
    LINK_INIT_NODE(FunctionDecl);
    if (LinkHeader(FunctionDecl) != LINK_OK)
        return LINK_FATAL;
    scope* OldScope = CurrentScope;
    CurrentScope = FunctionDecl->Scope;
    for (node* Node : FunctionDecl->Statements) {
        Node->Parent = FunctionDecl;
        if (!IsType(Node->AstType))
            continue;
        if (LinkNode(Node) != LINK_OK)
            return LINK_FATAL;
    }
    bool HasReturnStatement = false;
    int WorstResult = 0;
    for (node* Node : FunctionDecl->Statements) {
        if (IsType(Node->AstType))
            continue;
        WorstResult = Max(WorstResult, LinkNode(Node));
        if (WorstResult == LINK_FATAL) {
            return LINK_FATAL;
        }
        if (Node->AstType == NodeType_Return)
            HasReturnStatement = true;
    }
    // TODO: Function must return something
    //if (!FunctionDecl->IsInternal && !HasReturnStatement && FunctionDecl->ReturnType.Decl != Type_void && FunctionDecl->ReturnType.PointerLevel > 0) {
    //    ReportError(FunctionDecl->Location, "Function must return something");
    //    return LINK_ERROR;
    //}
    CurrentScope = OldScope;
    return WorstResult;
}
int parser::LinkNode(node_typedecl* TypeDecl) {
    LINK_INIT_NODE(TypeDecl);
    for (node* M : TypeDecl->Members) {
        M->Parent = TypeDecl;
        switch (M->AstType) {
            case NodeType_MemberVarDecl: {
                auto Member = (node_membervardecl*)M;
                SET_PARENT(Member, Declaration);
                auto MemberType = Member->Declaration->Type;
                auto MemberTypeIdentifier = GetTypeIdentifier(MemberType);
                MemberTypeIdentifier->Decl = FindTypeDecl(Member->Location, MemberTypeIdentifier->Identifier);
                if (!MemberTypeIdentifier->Decl)
                    return LINK_FATAL;
                if (LinkNode(MemberTypeIdentifier->Decl) != LINK_OK)
                    return LINK_FATAL;
                if (!Member->Offset.Custom) {
                    if (TypeDecl->Packing.Custom) {
                        if (TypeDecl->Packing.Value == 0) {
                            ReportError(TypeDecl->Packing.Expression->Location, "Zero packing??");
                            return LINK_FATAL;
                        }
                        else if (!IsPowerOf2(TypeDecl->Packing.Value)) {
                            ReportError(TypeDecl->Packing.Expression->Location, "Packing must be a power of two");
                            return LINK_FATAL;
                        }
                        Member->Offset.Value = Ceil(TypeDecl->Size, TypeDecl->Packing.Value);
                    }
                    else
                        Member->Offset.Value = Ceil(TypeDecl->Size, GetAlign(MemberType));
                }
                TypeDecl->Size = Max(TypeDecl->Size, Member->Offset.Value + GetSizeOf(MemberType));
                if (TypeDecl->Align.Custom) {
                    if (TypeDecl->Align.Value == 0) {
                        ReportError(TypeDecl->Align.Expression->Location, "Zero align??");
                        return LINK_FATAL;
                    }
                    else if (!IsPowerOf2(TypeDecl->Align.Value)) {
                        ReportError(TypeDecl->Align.Expression->Location, "Align must be a power of two");
                        return LINK_FATAL;
                    }
                }
                else
                    TypeDecl->Align.Value = Max(TypeDecl->Align.Value, GetAlign(MemberType));
                break;
            }
            case NodeType_MemberFuncDecl: {
                auto Member = (node_memberfuncdecl*)M;
                SET_PARENT(Member, Declaration);
                if (LinkNode(Member->Declaration) != LINK_OK) {
                    return LINK_FATAL;
                }
                break;
            }
            case NodeType_TypeDecl: {
                auto Type = (node_typedecl*)M;
                if (LinkNode(Type) != LINK_OK) {
                    return LINK_FATAL;
                }
                break;
            }
            default: {
                assert(0);
                break;
            }
        }
    }
    TypeDecl->Size = Max(TypeDecl->Size, TypeDecl->Align.Value);
    return LINK_OK;
}
int parser::LinkNode(node_typealias* TypeAlias) {
    LINK_INIT_NODE(TypeAlias);
    if (CurrentScope->Types[TypeAlias->NewIdentifier]) {
        ReportError(TypeAlias->Location, "Type '", TypeAlias->NewIdentifier, "' is already declared");
        return LINK_FATAL;
    }
    node_typedecl* Decl = FindTypeDecl(TypeAlias->Location, TypeAlias->OldIdentifier);
    if (Decl) {
        CurrentScope->Types[TypeAlias->NewIdentifier] = Decl;
        return LINK_OK;
    }
    else {
        return LINK_FATAL;
    }
}
int parser::LinkNode(node_identifier* Identifier) {
    LINK_INIT_NODE(Identifier);
    Identifier->Declaration = TryFindVarDecl(Identifier, Identifier->Identifier);
    if (!Identifier->Declaration) {
        Identifier->Declaration = TryFindNamespaceDecl(Identifier, Identifier->Identifier);
    }
    if (!Identifier->Declaration) {
        PrintNotDeclaredError(Identifier->Location, Identifier->Identifier);
        return LINK_FATAL;
    }
    if (Identifier->Declaration->AstType == NodeType_VarDecl) {
        //if (LinkNode(Identifier->Declaration) != LINK_OK)
        //    return LINK_FATAL;
        Identifier->Type = ((node_vardecl*)Identifier->Declaration)->Type;
    }
    return LINK_OK;
}
int parser::LinkNode(node_functioncall* Call) {
    LINK_INIT_NODE(Call);
    SearchScopeNodeStack.push(0);
    for (node* Arg : Call->Arguments) {
        Arg->Parent = Call;
        if (LinkNode(Arg) != LINK_OK) {
            return LINK_FATAL;
        }
    }
    SearchScopeNodeStack.pop();
    Call->Declaration = GetMatchingOverload(Call);
    if (!Call->Declaration) {
        return LINK_FATAL;
    }
    Call->Type = Call->Declaration->ReturnType;
    return LINK_OK;
}
int parser::LinkNode(node_memberaccess* MemberAccess) {
    LINK_INIT_NODE(MemberAccess);
    SET_PARENT(MemberAccess, First);
    SET_PARENT(MemberAccess, Second);
    if (LinkNode(MemberAccess->First) != LINK_OK)
        return LINK_FATAL;
    switch (MemberAccess->First->AstType) {
        case NodeType_Identifier: {
            auto Decl = GetDeclaration(MemberAccess->First);
            switch (Decl->AstType) {
                case NodeType_VarDecl:
                    // TODO deal with pointers
                    SearchScopeNodeStack.push(GetType(Decl));
                    break;
                case NodeType_NSDecl:
                    SearchScopeNodeStack.push(Decl);
                    break;
                default:
                    assert(0);
                    return 0;
            }
            break;
        }
        default: {
            SearchScopeNodeStack.push(GetType(MemberAccess->First));
            break;
        }
    }
    if (LinkNode(MemberAccess->Second) != LINK_OK)
        return LINK_FATAL;
    SearchScopeNodeStack.pop();
    MemberAccess->Type = MemberAccess->Second->Type;
    return LINK_OK;
}
int parser::LinkNode(node_sizeof* Sizeof) {
    LINK_INIT_NODE(Sizeof);
    auto Type = GetTypeIdentifier(Sizeof->TestType);
    Type->Decl = FindTypeDecl(Sizeof->Location, Type->Identifier);
    if (!Type->Decl)
        return LINK_FATAL;
    Sizeof->Flags |= AstFlag_Known;
    return LINK_OK;
}
int parser::LinkNode(node_paren* Paren) {
    LINK_INIT_NODE(Paren);
    SET_PARENT(Paren, Expression);
    int Result = LinkNode(Paren->Expression);
    Paren->Flags &= ~AstFlag_Known;
    Paren->Flags |= Paren->Expression->Flags & AstFlag_Known;
    Paren->Type = Paren->Expression->Type;
    return Result;
}
int parser::LinkNode(node_literal* Literal) {
    LINK_INIT_NODE(Literal);
    Literal->Type = [&]() {
        switch (Literal->LiteralType) {
            case data_type::Bool: return TypeIdentifier_bool;
            case data_type::Char: return TypeIdentifier_char;
            case data_type::Float: return TypeIdentifier_float32;
            case data_type::UInt:
                if (Literal->Value.Int > UINT32_MAX)
                    return TypeIdentifier_uint64;
                if (Literal->Value.Int > UINT16_MAX)
                    return TypeIdentifier_uint32;
                if (Literal->Value.Int > UINT8_MAX)
                    return TypeIdentifier_uint16;
                return TypeIdentifier_uint8;
            case data_type::String: return TypeIdentifier_string;
            default: assert(0);
        }
    }();
    return LINK_OK;
}
int parser::LinkNode(node_binaryop* Binop) {
    LINK_INIT_NODE(Binop);
    SET_PARENT(Binop, FirstExpression);
    SET_PARENT(Binop, SecondExpression);
    if (LinkNode(Binop->FirstExpression) != LINK_OK) {
        return LINK_FATAL;
    }
    if (LinkNode(Binop->SecondExpression) != LINK_OK) {
        return LINK_FATAL;
    }
    auto FirstExpr= Binop->FirstExpression;
    auto SecondExpr= Binop->SecondExpression;
    node_type* FirstType = FirstExpr->Type;
    node_type* SecondType = SecondExpr->Type;
    switch (GetBinopType(Binop->Op)) {
        case binop_type::Comparison:
        case binop_type::Equality: {
            auto Ptr0 = [this](node_expression* A, node_expression* B) {
                return IsPointer(A->Type) && B->Flags & AstFlag_Known && IsInteger(B->Type) && EvaluateInt(B) == 0;
            };
            if ((IsPointer(FirstType) && IsPointer(SecondType) && Equals(FirstType, SecondType)) ||
                Ptr0(FirstExpr, SecondExpr) ||
                Ptr0(SecondExpr, FirstExpr)) 
            {
                Binop->Type = TypeIdentifier_bool;
                return LINK_OK;
            }
            break;
        }
    }
    if (Binop->Op == binop::Add) {
        if (IsPointer(FirstType) && IsInteger(SecondType)) {
            Binop->Type = FirstType;
            return LINK_OK;
        }
        else if (IsInteger(FirstType) && IsPointer(SecondType)) {
            Binop->Type = SecondType;
            return LINK_OK;
        }
    }
    else if (Binop->Op == binop::AddEq) {
        if (IsPointer(FirstType) && IsInteger(SecondType)) {
            Binop->Type = FirstType;
            return LINK_OK;
        }
    }
    else if (Binop->Op == binop::Sub) {
        if (IsPointer(FirstType) && IsPointer(SecondType) && Equals(FirstType, SecondType)) {
            Binop->Type = TypeIdentifier_int;
            return LINK_OK;
        }
    }
    list<node_functiondecl*> PossibleOverloads;
    for (auto Decl : GlobalNode->BinaryOperators[ToInt(Binop->Op)]) {
        Decl->Parent = GlobalNode;
        if (LinkHeader(Decl) != LINK_OK)
            return LINK_FATAL;
        if (!((Binop->FirstExpression->Flags & AstFlag_Known) ^ (Binop->SecondExpression->Flags & AstFlag_Known))) {
            if (ImplicitlyConvertible(Binop->FirstExpression, Decl->Arguments[0]->Type)) {
                if (ImplicitlyConvertible(Binop->SecondExpression, Decl->Arguments[1]->Type)) {
                    PossibleOverloads.push_back(Decl);
                }
            }
        }
        else if (Binop->FirstExpression->Flags & AstFlag_Known) {
            if (ImplicitlyConvertible(Binop->FirstExpression, Decl->Arguments[0]->Type)) {
                if (Equals(Binop->SecondExpression->Type, Decl->Arguments[1]->Type)) {
                    PossibleOverloads.push_back(Decl);
                }
            }
        }
        else if (Binop->SecondExpression->Flags & AstFlag_Known) {
            if (Equals(Binop->FirstExpression->Type, Decl->Arguments[0]->Type)) {
                if (ImplicitlyConvertible(Binop->SecondExpression, Decl->Arguments[1]->Type)) {
                    PossibleOverloads.push_back(Decl);
                }
            }
        }
        else {
            assert(0);
        }
    }
    if (PossibleOverloads.size() == 0) {
        ReportError(Binop->Location, "operator", BinopString(Binop->Op), "(", FirstType, ", ", SecondType, ") does not exist");
        return LINK_FATAL;
    }
    else if (PossibleOverloads.size() > 1) {
        for (auto Decl : PossibleOverloads) {
            if (Equals(FirstType, Decl->Arguments[0]->Type) && Equals(SecondType, Decl->Arguments[1]->Type)) {
                Binop->Declaration = Decl;
                break;
            }
        }
        if (!Binop->Declaration) {
            for (auto Decl : PossibleOverloads) {
                if (ImplicitlyConvertible(FirstExpr, Decl->Arguments[0]->Type) && ImplicitlyConvertible(SecondExpr, Decl->Arguments[1]->Type)) {
                    Binop->Declaration = Decl;
                    break;
                }
            }
        }
        if (!Binop->Declaration) {
            ReportError(Binop->Location, "Binary operator", BinopString(Binop->Op), "(", FirstType, ", ", SecondType, ") is ambiguous");
            ReportMessage("    Matching overloads:");
            for (auto Decl : PossibleOverloads) {
                ReportMessage("        operator", Decl->Identifier, "(", Decl->Arguments[0]->Type, ", ", Decl->Arguments[1]->Type, "): ", Decl->ReturnType);
            }
            ReportMessage();
            return LINK_FATAL;
        }
    }
    else
        Binop->Declaration = PossibleOverloads[0];
    Binop->Type = Binop->Declaration->ReturnType;
    return LINK_OK;
}
int parser::LinkNode(node_return* Return) {
    LINK_INIT_NODE(Return);
    SET_PARENT(Return, Expression);
    if (LinkNode(Return->Expression) != LINK_OK)
        return LINK_FATAL;
    node* SearchNode = Return->Parent;
    while (SearchNode && SearchNode->AstType != NodeType_FunctionDecl)
        SearchNode = SearchNode->Parent;
    node_functiondecl* FDecl = (node_functiondecl*)SearchNode;
    auto RetExprType = Return->Expression->Type;
    if (!ImplicitlyConvertible(Return->Expression, FDecl->ReturnType)) {
        ReportError(Return->Location, "Return type '", RetExprType, "' does not match with ", FDecl->Identifier, "'s return type '", FDecl->ReturnType, "'");
        return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_nsdecl* NSDecl) {
    LINK_INIT_NODE(NSDecl);
    //std::sort(NSDecl->Declarations.begin(), NSDecl->Declarations.end(), [](node* A, node* B) {
    //    return GetPriority(A) < GetPriority(B);
    //});
    for (auto Decl : NSDecl->Declarations) {
        Decl->Parent = NSDecl;
        if (LinkNode(Decl) == LINK_FATAL)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_if* If) {
    LINK_INIT_NODE(If);
    SET_PARENT(If, Condition);
    if (LinkNode(If->Condition) != LINK_OK)
        return LINK_FATAL;
    if (GetTypeDecl(If->Condition) != Type_bool) {
        ReportError(If->Condition->Location, "Expression type must be a bool");
        return LINK_FATAL;
    }
    for (auto S : If->Statements) {
        S->Parent = If;
        if (LinkNode(S) == LINK_FATAL)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_else* Else) {
    LINK_INIT_NODE(Else);
    for (auto S : Else->Statements) {
        S->Parent = Else;
        if (LinkNode(S) == LINK_FATAL)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_while* While) {
    LINK_INIT_NODE(While);
    SET_PARENT(While, Condition);
    if (LinkNode(While->Condition) != LINK_OK)
        return LINK_FATAL;
    if (GetTypeDecl(While->Condition) != Type_bool) {
        ReportError(While->Condition->Location, "Expression type must be a bool");
        return LINK_FATAL;
    }
    for (auto S : While->Statements) {
        S->Parent = While;
        if (LinkNode(S) == LINK_FATAL)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node_address* Address) {
    LINK_INIT_NODE(Address);
    SET_PARENT(Address, Expression);
    if (LinkNode(Address->Expression) != LINK_OK)
        return LINK_FATAL;
    if (!CanTakeAddressOf(Address->Expression)) {
        ReportError(Address->Location, "Can't take adress of this expression");
        return LINK_FATAL;
    }
    Address->Type = CreateTypePointer(Address->Expression->Type);
    return LINK_OK;
}
int parser::LinkNode(node_deref* Deref) {
    LINK_INIT_NODE(Deref);
    SET_PARENT(Deref, Expression);
    if (LinkNode(Deref->Expression) != LINK_OK)
        return LINK_FATAL;
    if (!IsPointer(Deref->Expression->Type)) {
        ReportError(Deref->Location, "Can't dereference not a pointer");
        return LINK_FATAL;
    }
    Deref->Type = ((node_type_pointer*)Deref->Expression->Type)->Next;
    return LINK_OK;
}
int parser::LinkNode(node_index* Index) {
    LINK_INIT_NODE(Index);
    SET_PARENT(Index, Expression);
    SET_PARENT(Index, IndexExpression);
    if (LinkNode(Index->Expression) != LINK_OK)
        return LINK_FATAL;
    if (LinkNode(Index->IndexExpression) != LINK_OK)
        return LINK_FATAL;
    if (!IsArray(Index->Expression->Type)) {
        ReportError(Index->Expression->Location, "Can't index not an array");
        return LINK_FATAL;
    }
    if (!IsInteger(Index->IndexExpression->Type)) {
        ReportError(Index->IndexExpression->Location, "operator[] must get integer!");
        return LINK_FATAL;
    }
    Index->Type = ((node_type_array*)Index->Expression->Type)->Next;
    return LINK_OK;
}
int parser::LinkNode(node_not* Not) {
    LINK_INIT_NODE(Not);
    SET_PARENT(Not, Expression);
    if (LinkNode(Not->Expression) != LINK_OK)
        return LINK_FATAL;
    if (!Equals(GetType(Not->Expression), TypeIdentifier_bool)) {
        ReportError(Not->Location, "Not operator '!' can be applied only to bool expression");
        return LINK_FATAL;
    }
    Not->Type = TypeIdentifier_bool;
    return LINK_OK;
}
int parser::LinkNode(node_negate* Negate) {
    LINK_INIT_NODE(Negate);
    SET_PARENT(Negate, Expression);
    if (LinkNode(Negate->Expression) != LINK_OK)
        return LINK_FATAL;

    auto PrintError = [&]() {
        ReportError(Negate->Location, "operator-(", Negate->Expression->Type, ") does not exist");
    };

    if (Negate->Expression->Flags & AstFlag_Known) {
        if (IsInteger(Negate->Expression)) {
            auto Val = -EvaluateInt(Negate->Expression);
            Negate->Type = GetFittingIntType(Val);
        }
        else {
            PrintError();
            return LINK_FATAL;
        }
    }
    else {
        for (auto Decl : GlobalNode->NegateOperators) {
            Decl->Parent = GlobalNode;
            if (LinkHeader(Decl) != LINK_OK)
                return LINK_FATAL;
            if (Equals(Negate->Expression->Type, Decl->Arguments[0]->Type)) {
                Negate->Declaration = Decl;
            }
        }
        if (!Negate->Declaration) {
            PrintError();
            return LINK_FATAL;
        }
        Negate->Type = Negate->Declaration->ReturnType;
    }
    SetFlag(Negate->Flags, Negate->Expression->Flags, AstFlag_Known);
    return LINK_OK;
}
int parser::LinkNode(node_as* As) {
    LINK_INIT_NODE(As);
    SET_PARENT(As, Expression);
    if (LinkNode(As->Expression) != LINK_OK)
        return LINK_FATAL;
    auto Type = GetTypeIdentifier(As->Type);
    Type->Decl = FindTypeDecl(As->Location, Type->Identifier);
    if (!Type->Decl)
        return LINK_FATAL;

    if (IsPointer(As->Type)) {
        if (IsPointer(As->Expression)) {
            return LINK_OK;
        }
        if ((As->Expression->Flags & AstFlag_Known) && IsInteger(As->Expression) && EvaluateInt(As->Expression) == 0) {
            return LINK_OK;
        }
    }

    list<node_functiondecl*> PossibleOverloads;
    for (auto Decl : GlobalNode->AsOperators) {
        Decl->Parent = GlobalNode;
        if (LinkHeader(Decl) != LINK_OK)
            return LINK_FATAL;
        if (Equals(Decl->ReturnType, As->Type) && Equals(As->Expression->Type, Decl->Arguments[0]->Type)) {
            PossibleOverloads.push_back(Decl);
        }
    }
    if (PossibleOverloads.size() == 0) {
        ReportError(As->Location, "operator as(", As->Expression->Type, "): ", As->Type, " does not exist");
        return LINK_FATAL;
    }
    else if (PossibleOverloads.size() > 1) {
        assert(0);
        /*for (auto Decl : PossibleOverloads) {
            if (Decl->ReturnType == As->Type && Equals(As->Expression->Type, Decl->Arguments[0]->Type)) {
                As->Decl = Decl;
                break;
            }
        }
        if (!As->Decl) {
            ReportError(As->Location, "operator as(", As->Expression->Type, ") is ambiguous");
            ReportMessage("    Matching overloads:");
            for (auto Decl : PossibleOverloads) {
                ReportMessage("        operator as", Decl->Identifier, "(", Decl->Arguments[0]->Type, "): ", Decl->ReturnType);
            }
            ReportMessage();
            return LINK_FATAL;
        }*/
    }
    else {
        As->Decl = PossibleOverloads[0];
    }

    return LINK_OK;
}
int parser::LinkNode(node_break* Break) {
    LINK_INIT_NODE(Break);
    node* BreakHolder = GetNextBreakHolder(Break);
    if (!BreakHolder) {
        ReportError(Break->Location, "break statement can not be declared here.");
        return LINK_ERROR;
    }
    Break->Statement = BreakHolder;
    return LINK_OK;
}
int parser::LinkNode(node_continue* Continue) {
    LINK_INIT_NODE(Continue);
    node* ContinueHolder = GetNextContinueHolder(Continue);
    if (!ContinueHolder) {
        ReportError(Continue->Location, "continue statement can not be declared here.");
        return LINK_ERROR;
    }
    Continue->Statement = ContinueHolder;
    return LINK_OK;
}
int parser::LinkNode(node_braceinit* Braceinit) {
    LINK_INIT_NODE(Braceinit);
    switch (Braceinit->Parent->AstType) {
        case NodeType_VarDecl: {
            auto VarDecl = (node_vardecl*)Braceinit->Parent;
            if (VarDecl->Type == TYPE_AUTO) {
                ReportError(Braceinit->Location, "Can't deduce type!");
                return LINK_ERROR;
            }
            Braceinit->Type = VarDecl->Type;
            break;
        }
        case NodeType_Assignment: {
            auto Assignment = (node_assignment*)Braceinit->Parent;
            if (Braceinit == Assignment->Assigned) {
                ReportError(Braceinit->Location, "WTF???");
                return LINK_ERROR;
            }
            Braceinit->Type = Assignment->Assigned->Type;
            break;
        }
        default:
            ReportError(Braceinit->Location, "Brace init cant be here");
            return LINK_ERROR;
    }
    return LINK_OK;
}
int parser::LinkNode(node_defer* Defer) {
    LINK_INIT_NODE(Defer);
    for (auto S : Defer->Statements) {
        S->Parent = Defer;
        if (LinkNode(S) == LINK_FATAL)
            return LINK_FATAL;
    }
    return LINK_OK;
}
int parser::LinkNode(node* Node) {
    switch (Node->AstType) {
        case NodeType_Literal:      return LinkNode((node_literal*)Node);
        case NodeType_FunctionDecl: return LinkNode((node_functiondecl*)Node);
        case NodeType_VarDecl:      return LinkNode((node_vardecl*)Node);
        case NodeType_Assignment:   return LinkNode((node_assignment*)Node);
        case NodeType_FunctionCall: return LinkNode((node_functioncall*)Node);
        case NodeType_TypeDecl:     return LinkNode((node_typedecl*)Node);
        case NodeType_TypeAlias:    return LinkNode((node_typealias*)Node);
        case NodeType_Identifier:   return LinkNode((node_identifier*)Node);
        case NodeType_MemberAccess: return LinkNode((node_memberaccess*)Node);
        case NodeType_Sizeof:       return LinkNode((node_sizeof*)Node);
        case NodeType_Paren:        return LinkNode((node_paren*)Node);
        case NodeType_BinaryOp:     return LinkNode((node_binaryop*)Node);
        case NodeType_Return:       return LinkNode((node_return*)Node);
        case NodeType_NSDecl:       return LinkNode((node_nsdecl*)Node);
        case NodeType_If:           return LinkNode((node_if*)Node);
        case NodeType_Else:         return LinkNode((node_else*)Node);
        case NodeType_Address:      return LinkNode((node_address*)Node);
        case NodeType_Deref:        return LinkNode((node_deref*)Node);
        case NodeType_While:        return LinkNode((node_while*)Node);
        case NodeType_Index:        return LinkNode((node_index*)Node);
        case NodeType_Negate:       return LinkNode((node_negate*)Node);
        case NodeType_Not:          return LinkNode((node_not*)Node);
        case NodeType_As:           return LinkNode((node_as*)Node);
        case NodeType_Break:        return LinkNode((node_break*)Node);
        case NodeType_Continue:     return LinkNode((node_continue*)Node);
        case NodeType_BraceInit:    return LinkNode((node_braceinit*)Node);
        case NodeType_Defer:        return LinkNode((node_defer*)Node);
        default:
            assert(0);
    }
}

inline int parser::GetPossibleFunctionDecls(node_functioncall* Call, list<node_functiondecl*>& Result, u32 Flags) {
    auto Search = [&](node* Where) {
        list<node_functiondecl*> Candidates;
        for (node* TestNode : *GetChildren(Where)) {
            node_functiondecl* Decl = 0;
            if (TestNode->AstType == NodeType_FunctionDecl) {
                Decl = (node_functiondecl*)TestNode;
            }
            else if (TestNode->AstType == NodeType_MemberFuncDecl) {
                Decl = ((node_memberfuncdecl*)TestNode)->Declaration;
            }
            if (Decl) {
                if (Decl->Identifier == Call->Identifier) {
                    Candidates.push_back(Decl);
                }
            }
        }
        for (auto Decl : Candidates) {
            // HACK: Link all headers earlier
            Decl->Parent = Where;
            if (LinkHeader(Decl) != LINK_OK)
                return LINK_FATAL;
            //////////
        }

        if (Flags & GPFD_NO_ARGS) {
            Result.insert(Result.end(), Candidates.begin(), Candidates.end());
            return LINK_OK;
        }

        if (Flags & GPFD_STRICT)
            GetOverloadMatchingArguments(Result, Candidates, Call->Arguments, Equals);
        else {
            bool (parser:: * fun)(node_expression*, node_expression*) = &parser::ImplicitlyConvertible;
            GetOverloadMatchingArguments(Result, Candidates, Call->Arguments, std::bind(this, fun));
        }

        return LINK_OK;
    };
    auto SearchScopeNode = GetSearchScopeNode();
    if (SearchScopeNode) {
        if (Search(SearchScopeNode) != LINK_OK)
            return LINK_FATAL;
    }
    else {
        node* SearchNode = GetNextFunctionHolder(Call);
        while (true) {
            if (Search(SearchNode) != LINK_OK)
                return LINK_FATAL;
            SearchNode = GetNextFunctionHolder(SearchNode);
            if (!SearchNode) {
                break;
            }
        }
    }
    return LINK_OK;
}

inline node_functiondecl* parser::GetMatchingOverload(node_functioncall* Call) {
#if 0
    list<node_functiondecl*> PossibleOverloads;
    if (GetPossibleFunctionDecls(Call, PossibleOverloads, 0) != LINK_OK) {
        LinkResult = LINK_FATAL;
        return 0;
    }
    if (PossibleOverloads.size() == 0) {
        PossibleOverloads.clear();
        if (GetPossibleFunctionDecls(Call, PossibleOverloads, GPFD_NO_ARGS) != LINK_OK) {
            LinkResult = LINK_FATAL;
            return 0;
        }
        PrintNotDeclaredError(Call->Location, MakeHeaderString(Call));
        if (PossibleOverloads.size() != 0) {
            ReportMessage("    Available overloads:");
            for (auto Decl : PossibleOverloads) {
                ReportMessage("        ", MakeHeaderString(Decl));
            }
            ReportMessage();
        }
        LinkResult = LINK_FATAL;
        return 0;
    }
    else if (PossibleOverloads.size() > 1) {
        for (auto Decl : PossibleOverloads) {
            bool Match = true;
            for (int i=0; i < Call->Arguments.size(); ++i) {
                if (Call->Arguments[i]->Flags & AstFlag_Known) {
                    if (IsInteger(Call->Arguments[i]->Type)) {
                        auto Eval = EvaluateInt(Call->Arguments[i]);
                        if (Eval < 0) {
                            for (auto Decl : PossibleOverloads) {
                                if (IsSigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                    Call->Declaration = Decl;
                                    goto End;
                                }
                            }
                        }
                        else {
                            for (auto Decl : PossibleOverloads) {
                                if (IsUnsigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                    Call->Declaration = Decl;
                                    goto End;
                                }
                            }
                            for (auto Decl : PossibleOverloads) {
                                if (IsSigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                    Call->Declaration = Decl;
                                    goto End;
                                }
                            }
                        }
                        goto End;
                    }
                }
                if (!Equals(Call->Arguments[i]->Type, Decl->Arguments[i]->Type)) {
                    Match = false;
                    break;
                }
            }
            if (Match) {
                Call->Declaration = Decl;
                break;
            }
        }
    End:
        if (!Call->Declaration) {
            ReportError(Call->Location, "Function '", Call->Identifier, "' is ambiguous");
            ReportMessage("    Matching overloads:");
            for (auto Decl : PossibleOverloads) {
                ReportMessage("        ", MakeHeaderString(Decl));
            }
            ReportMessage();
            LinkResult = LINK_FATAL;
            return 0;
        }
    }
    else
        Call->Declaration = PossibleOverloads[0];
#endif
    list<node_functiondecl*> Overloads;
    auto SearchInNode = [&](node* Where, auto&& Predicate) {
        for (node* TestNode : *GetChildren(Where)) {
            node_functiondecl* Decl = 0;
            if (TestNode->AstType == NodeType_FunctionDecl) {
                Decl = (node_functiondecl*)TestNode;
            }
            else if (TestNode->AstType == NodeType_MemberFuncDecl) {
                Decl = ((node_memberfuncdecl*)TestNode)->Declaration;
            }
            if (Decl) {
                if (Predicate(Decl)) {
                    Overloads.push_back(Decl);
                }
            }
        }
        //for (auto Decl : Candidates) {
        //    // HACK: Link all headers earlier
        //    Decl->Parent = Where;
        //    if (LinkHeader(Decl) != LINK_OK)
        //        return LINK_FATAL;
        //    //////////
        //}
    };
    auto IdentifierMatcher = [&](node_functiondecl* Decl) {
        return Decl->Identifier == Call->Identifier;
    };
    auto Matcher = [&](node_functiondecl* Decl) {
        if (Decl->Identifier != Call->Identifier)
            return false;
        if (Decl->Arguments.size() == Call->Arguments.size()) {
            if (LinkHeader(Decl) != LINK_OK)
                assert(0);
            for (int i = 0; i < Call->Arguments.size(); ++i) {
                if (!ImplicitlyConvertible(Call->Arguments[i], Decl->Arguments[i]->Type))
                    return false;
            }
            return true;
        }
        else if (Decl->Arguments.size() > Call->Arguments.size()) {
            if (HasDefaultArguments(Decl)) {
                int i=0;
                for (; i < Call->Arguments.size(); ++i) {
                    if (!ImplicitlyConvertible(Call->Arguments[i], Decl->Arguments[i]->Type))
                        return false;
                }
                for (; i < Decl->Arguments.size(); ++i) {
                    if (!Decl->Arguments[i]->InitialExpression)
                        return false;
                }
                return true;
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    };
    auto SearchScopeNode = GetSearchScopeNode();
    auto Search = [&](auto&& Predicate) {
        if (SearchScopeNode) {
            SearchInNode(SearchScopeNode, Predicate);
        }
        else {
            node* SearchNode = GetNextFunctionHolder(Call);
            while (true) {
                SearchInNode(SearchNode, Predicate);
                SearchNode = GetNextFunctionHolder(SearchNode);
                if (!SearchNode) {
                    break;
                }
            }
        }
    };

    Search(Matcher);
    if (Overloads.size() == 0) {
        Search(IdentifierMatcher);
        PrintNotDeclaredError(Call->Location, MakeHeaderString(Call));
        if (Overloads.size() != 0) {
            ReportMessage("    Available overloads:");
            for (auto Decl : Overloads) {
                ReportMessage("        ", MakeHeaderString(Decl));
            }
            ReportMessage();
        }
    }
    else if (Overloads.size() > 1) {
        ReportError(Call->Location, "Function '", Call->Identifier, "' is ambiguous");
        ReportMessage("    Matching overloads:");
        for (auto Decl : Overloads) {
            ReportMessage(message::column(48), Decl->Location, ": ", MakeHeaderString(Decl));
        }
        ReportMessage();
    }
    else
        return Overloads[0];
    return 0;
}

inline node_typedecl* parser::FindTypeDecl(location Location, string_view TypeIdentifier) {
    node_typedecl* Declaration = TryFindTypeDecl(TypeIdentifier);
    if (!Declaration) {
        ReportError(Location, "Type '", TypeIdentifier, "' is not declared");
    }
    return Declaration;
}

inline node_vardecl* parser::FindVarDecl(node* ExprNode, location Location, string_view Identifier) {
    auto Result = TryFindVarDecl(ExprNode, Identifier);
    if (!Result) {
        ReportError(Location, "Variable '", Identifier, "' is not declared");
    }
    return Result;
}

inline node_nsdecl* parser::FindNamespaceDecl(node* ExprNode, location Location, string_view Identifier) {
    auto Result = TryFindNamespaceDecl(ExprNode, Identifier);
    if (!Result) {
        ReportError(Location, "Namespace '", Identifier, "' is not declared");
    }
    return Result;
}

bool parser::EnsureValid(node_type* Type) {
    if (!Type) {
        ReportError(TOKEN.Location, "Failed to parse type");
    }
    return Type != 0;
}
void parser::PrintNotDeclaredError(location Location, string_view Identifier) {
    auto SearchScopeNode = GetSearchScopeNode();
    if (SearchScopeNode) {
        switch (SearchScopeNode->AstType) {
            case NodeType_TypeDecl:
                ReportError(Location, "'", Identifier, "' is not a member of '", ((node_typedecl*)SearchScopeNode)->Identifier, "'");
                break;
            case NodeType_NSDecl:
                ReportError(Location, "'", Identifier, "' is not declared in '", ((node_nsdecl*)SearchScopeNode)->Identifier, "'");
                break;
            case NodeType_Type:
                ReportError(Location, "'", Identifier, "' is not a member of '", (node_type*)SearchScopeNode, "'");
                break;
            default:
                assert(0);
                break;
        }
    }
    else {
        ReportError(Location, "Identifier '", Identifier, "' is not declared");
    }
}
parser::found_import parser::FindImportFile(string_view Name) {
    found_import Result;
    for (auto& Dir : ImportDirectories) {
        Result.FullPath.Append(Dir.data(), Dir.size());
        Result.FullPath.Append(Name.Begin, Name.Count());
        Result.FullPath[Dir.size() + Name.Count()] = 0;
        Result.File = fopen(Result.FullPath.Begin, "rb");
        if (Result.File)
            return Result;
    }
    return {};
}

bool parser::ImplicitlyConvertible(node_expression* Src, node_expression* Dst) {
    return ImplicitlyConvertible(Src->Type, Dst->Type, Src);
}
bool parser::ImplicitlyConvertible(node_expression* Src, node_type* DstType) {
    return ImplicitlyConvertible(Src->Type, DstType, Src);
}
bool parser::ImplicitlyConvertible(node_type* SrcType, node_type* DstType, node* SrcNode) {
    NotConvertibleReason = 0;
    if (SrcType->Type == type::Identifier && DstType->Type == type::Identifier) {
        auto Src = (node_type_identifier*)SrcType;
        auto Dst = (node_type_identifier*)DstType;
        if (Src->Decl == Dst->Decl) {
            return true;
        }
        else {
            if (SrcNode->Flags & AstFlag_Known) {
                if (IsInteger(Dst->Decl) && IsInteger(Src->Decl)) {
                    EvaluatedValue = EvaluateInt(SrcNode);
                    if (Fits(EvaluatedValue, Dst->Decl))
                        return true;
                    else {
                        NotConvertibleReason = NCR_DOES_NOT_FIT;
                        return false;
                    }
                }
                else {
                    return false;
                }
            }
            else {
                if (IsInteger(Src) && IsInteger(Dst)) {
                    if (Fits(Src->Decl, Dst->Decl))
                        return true;
                    else {
                        NotConvertibleReason = NCR_DOES_NOT_FIT;
                        return false;
                    }
                }
                else {
                    return false;
                }
            }
        }
    }
    else {
        if (IsPointer(DstType)) {
            auto DstPtr = (node_type_pointer*)DstType;
            if (IsInteger(SrcType)) {
                return SrcNode->AstType == NodeType_Literal && ((node_literal*)SrcNode)->Value.Int == 0;
            }
            else if (IsPointer(SrcType)) {
                return Equals(DstPtr->Next, TypeIdentifier_void) || Equals(SrcType, DstType);
            }
            else {
                return false;
            }
        }
        else {
            return Equals(SrcType, DstType);
        }
    }
}
bool parser::EnsureImplicitlyConvertible(node_expression* Src, node_expression* Dst, location Location) {
    return EnsureImplicitlyConvertible(Src->Type, Dst->Type, Src, Location);
}
bool parser::EnsureImplicitlyConvertible(node_expression* Src, node_type* DstType, location Location) {
    return EnsureImplicitlyConvertible(Src->Type, DstType, Src, Location);
}
bool parser::EnsureImplicitlyConvertible(node_type* SrcType, node_type* DstType, node* SrcNode, location Location) {
    if (ImplicitlyConvertible(SrcType, DstType, SrcNode)) {
        return true;
    }
    switch (NotConvertibleReason) {
        case NCR_DOES_NOT_FIT:
            ReportError(Location, "Can't convert from '", SrcType, "' to '", DstType, "'. Evaluated value '", EvaluatedValue, "' does not fit into destination range");
            break;
        default:
            ReportError(Location, "Can't convert from '", SrcType, "' to '", DstType, "'");
            break;
    }
    return false;
}
bool parser::IsIdentifierValid(string_view Identifier) {
    return
        Identifier != "sizeof" &&
        Identifier != "type";
}

arch CreateArch(string_view Name, location Location) {
    arch Result;
    if (Name == "x86") {
        Result.Type = arch_type::x86;
        Result.PtrSize = 4;
    }
    else if (Name == "x64") {
        Result.Type = arch_type::x64;
        Result.PtrSize = 8;
    }
    else {
        ReportError(Location, "Available arch are: 'x86', 'x64'; got '", Name, "'");
    }
    return Result;
}
