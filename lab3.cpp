#include <map>
#include <iostream>
#include <fstream>

#include <nlohmann/json.hpp>

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Verifier.h"

using nlohmann::json;

std::map<std::string, llvm::Value *> variables;

void codegen(json &ast);

void generate_blocks(json &ast);

void generate_declaration(json &ast);

void generate_assignment(json &ast);

llvm::Value *generate_arg(json &ast);

llvm::Value *generate_cond(json &ast);

llvm::Value *generate_expr(json &ast);

void generate_if(json &ast);

void generate_while(json &ast);

void generate_return(json &ast);

static llvm::LLVMContext ctx;
static llvm::Module *module = new llvm::Module("lab3", ctx);
static llvm::IRBuilder <llvm::NoFolder> builder(ctx);
static llvm::FunctionType *func_type = llvm::FunctionType::get(builder.getInt32Ty(), false);
static llvm::Function *main_func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "main", module);
static llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx, "entrypoint", main_func);

void generate_declaration(json &ast) {
    std::string name = ast["name"].get<std::string>();
    llvm::Value *v;
    if (ast.find("expr") != ast.end()) {
        v = generate_expr(ast["expr"]);
    } else {
        v = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
    }
    llvm::AllocaInst *Alloca = builder.CreateAlloca(llvm::Type::getInt32Ty(ctx), 0, name.c_str());
    builder.CreateStore(v, Alloca);
    builder.CreateLoad(Alloca);
    variables[name] = Alloca;
}

void generate_assignment(json &ast) {
    std::string name = ast["name"].get<std::string>();
    if (variables.find(name) == variables.end()) {
        std::cout << "var not exist" << std::endl;
        return;
    }
    llvm::Value *v = generate_expr(ast["expr"]);
    llvm::AllocaInst *Alloca = builder.CreateAlloca(llvm::Type::getInt32Ty(ctx), 0, name.c_str());
    builder.CreateStore(v, Alloca);
    builder.CreateLoad(Alloca);
    variables[name] = Alloca;
}

llvm::Value *generate_arg(json &ast) {
    llvm::Value *arg;
    if (ast.is_number()) {
        arg = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), ast.get<int>());
    } else {
        std::string name = ast.get<std::string>();
        llvm::Value *var = variables[name];
        if (!var) {
            std::cout << "var not exist" << std::endl;
        }
        arg = builder.CreateLoad(var, name.c_str());
    }
    return arg;
}

llvm::Value *generate_cond(json &ast) {
    std::string op = ast["op"].get<std::string>();
    llvm::Value *arg0 = generate_arg(ast["args"][0]);
    llvm::Value *arg1 = generate_arg(ast["args"][1]);
    if (op == ">") {
        return builder.CreateICmpSGT(arg0, arg1);
    } else if (op == ">=") {
        return builder.CreateICmpSGE(arg0, arg1);
    } else if (op == "<") {
        return builder.CreateICmpSLT(arg0, arg1);
    } else if (op == "<=") {
        return builder.CreateICmpSLE(arg0, arg1);
    } else if (op == "==") {
        return builder.CreateICmpEQ(arg0, arg1);
    } else if (op == "!=") {
        return builder.CreateICmpNE(arg0, arg1);
    } else {
        std::cout << "unhandled cond op" << std::endl;
        return nullptr;
    }
}

llvm::Value *generate_expr(json &ast) {
    if (ast.find("op") != ast.end()) {
        std::string op = ast["op"].get<std::string>();
        if (op == "+") {
            return builder.CreateAdd(generate_arg(ast["args"][0]), generate_arg(ast["args"][1]));
        } else if (op == "-") {
            return builder.CreateSub(generate_arg(ast["args"][0]), generate_arg(ast["args"][1]));
        } else if (op == "*") {
            return builder.CreateMul(generate_arg(ast["args"][0]), generate_arg(ast["args"][1]));
        } else if (op == "/") {
            return builder.CreateUDiv(generate_arg(ast["args"][0]), generate_arg(ast["args"][1]));
        } else {
            std::cout << "unhandled expr op" << std::endl;
            return nullptr;
        }
    } else {
        return generate_arg(ast["args"][0]);
    }
}

void generate_if(json &ast) {
    llvm::Value *cond = generate_cond(ast["cond"]);
    llvm::Function *main_f = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *prev = builder.GetInsertBlock();
    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(ctx, "then", main_f);
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(ctx, "else");
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(ctx, "post_if");

    if (ast.find("alt") != ast.end()) {
        builder.CreateCondBr(cond, then_bb, else_bb);
    } else {
        builder.CreateCondBr(cond, then_bb, merge_bb);
    }

    builder.SetInsertPoint(then_bb);

    generate_blocks(ast["conv"]);

    builder.CreateBr(merge_bb);
    then_bb = builder.GetInsertBlock();

    if (ast.find("alt") != ast.end()) {
        main_f->getBasicBlockList().push_back(else_bb);
        builder.SetInsertPoint(else_bb);
        generate_blocks(ast["alt"]);

        builder.CreateBr(merge_bb);
        else_bb = builder.GetInsertBlock();
        main_f->getBasicBlockList().push_back(merge_bb);
        builder.SetInsertPoint(merge_bb);
        llvm::PHINode *PN = builder.CreatePHI(llvm::Type::getInt32Ty(ctx), 2, "tmp_if");

        PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), then_bb);
        PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), else_bb);
    } else {
        main_f->getBasicBlockList().push_back(merge_bb);
        builder.SetInsertPoint(merge_bb);
        llvm::PHINode *PN = builder.CreatePHI(llvm::Type::getInt32Ty(ctx), 2, "tmp_if");

        PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), prev);
        PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), then_bb);
    }
}

void generate_while(json &ast) {
    llvm::Value *cond = generate_cond(ast["cond"]);
    llvm::Function *main_f = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *prev = builder.GetInsertBlock();
    llvm::BasicBlock *loop_bb = llvm::BasicBlock::Create(ctx, "loop", main_f);
    llvm::BasicBlock *post_loop = llvm::BasicBlock::Create(ctx, "post_loop", main_f);

    builder.CreateCondBr(cond, loop_bb, post_loop);

    builder.SetInsertPoint(loop_bb);

    generate_blocks(ast["body"]);

    builder.CreateCondBr(cond, loop_bb, post_loop);

    builder.SetInsertPoint(post_loop);

    llvm::PHINode *PN = builder.CreatePHI(llvm::Type::getInt32Ty(ctx), 2, "looptmp");
    PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), prev);
    PN->addIncoming(llvm::Constant::getNullValue(llvm::Type::getInt32Ty(ctx)), loop_bb);
}

void generate_return(json &ast) {
    llvm::Value *ret_value;
    if (ast["ret"].is_number()) {
        ret_value = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), ast["ret"].get<int>());
    } else {
        std::string name = ast["ret"].get<std::string>();
        llvm::Value *var = variables[name];
        if (!var) {
            std::cout << "var not exist" << std::endl;
        }
        ret_value = builder.CreateLoad(var, name.c_str());
    }
    builder.CreateRet(ret_value);
}

void generate_blocks(json &ast) {
    int size = ast.size();
    for (int i = 0; i < size; i++) {
        json block = ast[i];
        std::string block_type = block["type"].get<std::string>();
        if (block_type == "VarDeclaration") {
            generate_declaration(block["expr"]);
        } else if (block_type == "AssignExpr") {
            generate_assignment(block["expr"]);
        } else if (block_type == "IfStmt") {
            generate_if(block);
        } else if (block_type == "WhileStmt") {
            generate_while(block);
        } else {
            std::cout << "unknown block type" << std::endl;
        }
    }
}

void codegen(json &ast) {
    generate_blocks(ast["body"]);
    generate_return(ast);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cout << "cfg required" << std::endl;
    }
    // read config
    std::ifstream ifs(argv[1]);
    json ast = json::parse(ifs);
    // set entry
    builder.SetInsertPoint(entry);
    // start codegen
    codegen(ast);
    // dump to file
    module->dump();
    return 0;
}