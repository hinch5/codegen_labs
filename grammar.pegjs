Program
	= AnyWhitespace "int" _ "main()" _ "{" AnyWhitespace main:Main AnyWhitespace "}" AnyWhitespace { return main; }
  
Main
	= blocks:Body ret:Return { return {body: blocks, ret: ret} }

Body
	= blocks:((Block AnyWhitespace)*) { return blocks.map(b => b[0]); }

Block
	= block:(Declaration / Assignment / If / While)
  
Declaration
	= "int" _ name:Ident _ expr:("=" _ Expr)? ";" { 
        const res = {name: name};
        if (expr) {
            res.expr = expr[2];
        }
        return {
            type: 'VarDeclaration',
            expr: res
        };
        return res;
    }

Expr
	= arg0:(Ident / Number) _ binary:(Op _ (Ident / Number))? {
        const res = {
            args: [arg0]
        }
        if (binary) {
            res.args.push(binary[2]);
            res.op = binary[0];
        }
        return res;
    }

Assignment
	= name:Ident _ "=" _ expr:Expr ";" {
        return {
            type: "AssignExpr",
            expr: {
                name: name,
                expr: expr
            }
        }
    }

If
	= "if" _ "(" _ cond:Cond _ ")" _ "{" _ AnyWhitespace body:Body AnyWhitespace "}" _ alt:("else" _ "{" _ AnyWhitespace Body AnyWhitespace "}")? {
        const res = {
            type: "IfStmt",
            cond: cond,
            conv: body
        }
        if (alt) {
            res.alt = alt[5];
        }
        return res;
    }

While
	= "while" _ "(" _ cond:Cond _ ")" _ "{" _ AnyWhitespace body:Body AnyWhitespace "}" {
        return {
            type: "WhileStmt",
            cond: cond,
            body: body
        }
    }

Cond
	= arg0:(Ident / Number) _ op:CondOp _ arg1:(Ident / Number) {
        return {
            args: [arg0, arg1],
            op: op
        }
    }

Return
	= _ "return" _ value:(Ident / Number) _ ";" { return value; }

Op
	= ("+" / "-" / "*" / "/") { return text(); }

CondOp
	= (">="/ ">" / "<=" / "<" / "==" / "!=") { return text(); }

Ident
	= value:([A-Za-z][A-Za-z0-9]*) {return text(); }  
  
Number
	= value:[0-9]+ {return parseInt(text(), 10);}

AnyWhitespace
	= [ \t\r\n]*

_ "whitespace"
	= [ \t\r]*
    
