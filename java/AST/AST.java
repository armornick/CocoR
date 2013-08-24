package Taste;

import java.util.List;
import java.util.ArrayList;

enum Type { UNDEF, INT, BOOL }
enum Operator { EQU, LSS, GTR, ADD, SUB, MUL, DIV }

abstract class Node {  // node of the AST
	abstract void dump();
}

//----------- Declarations ----------------------------

class Obj extends Node { // any declared object that has a name
	String name;           // name of this object
	Type type;             // type of this object (UNDEF for procedures)
	Obj(String s, Type t) { name = s; type = t; }
	void dump() {}
}

class Var extends Obj {  // variable
	int adr;               // address in memory
	Var(String name, Type type) { super(name, type); }
}

class Proc extends Obj { // procedure (also used for the main program)
	List<Obj> locals;      // local objects declared in this procedures
	Block block;           // block of this procedure (null for the main program)
	int nextAdr;	         // next free address in this procedure
	Proc program;	         // link to the Proc node of the main program or null
	Parser parser;         // for error messages

	Proc(String name, Proc program, Parser parser) {
		super(name, Type.UNDEF);
		locals = new ArrayList<Obj>();
		this.program = program;
		this.parser = parser;
	}

	void add (Obj obj) {
		for (Obj o: locals) {
			if (o.name.equals(obj.name)) parser.SemErr(obj.name + " declared twice");
		}
		locals.add(obj);
		if (obj instanceof Var) ((Var)obj).adr = nextAdr++;
	}

	Obj find (String name) {
		for (Obj o: locals) { if (o.name.equals(name)) return o; }
		if (program != null) for (Obj o: program.locals) { if (o.name.equals(name)) return o; }
		parser.SemErr(name + " undeclared");
		return new Obj("_undef", Type.INT); // dummy
	}

	void dump() {
		System.out.println("Proc " + name); block.dump(); System.out.println();
	}
}

//----------- Expressions ------------------------------

abstract class Expr extends Node  {}

class BinExpr extends Expr {
	Operator op;
	Expr left, right;
	BinExpr(Expr e1, Operator o, Expr e2) { op = o; left = e1; right = e2; }
	void dump() { left.dump(); System.out.print(" " + op.name() + " "); right.dump(); }
}

class UnaryExpr extends Expr {
	Operator op;
	Expr e;
	UnaryExpr(Operator x, Expr y) { op = x; e = y; }
	void dump() { System.out.print(op.name() + " "); e.dump(); }
}

class Ident extends Expr {
	Obj obj;
	Ident(Obj o) { obj = o; }
	void dump() { System.out.print(obj.name); }
}

class IntCon extends Expr {
	int val;
	IntCon(int x) { val = x; }
	void dump() { System.out.print(val); }
}

class BoolCon extends Expr {
	boolean val;
	BoolCon(boolean x) { val = x; }
	void dump() { System.out.print(val); }
}

//------------- Statements -----------------------------

class Stat extends Node {
	static int indent = 0;
	void dump() { for (int i = 0; i < indent; i++) System.out.print("  "); }
}

class Assignment extends Stat {
	Obj  left;
	Expr right;
	Assignment(Obj o, Expr e) { left = o; right = e; }
	void dump() { super.dump(); System.out.print(left.name + " = "); right.dump(); System.out.println(); }
}

class Call extends Stat {
	Obj proc;
	Call(Obj o) { proc = o; }
	void dump() { super.dump(); System.out.println("call " + proc.name); }
}

class If extends Stat {
	Expr cond;
	Stat stat;
	If(Expr e, Stat s) { cond = e; stat = s; }
	void dump() { super.dump(); System.out.print("if "); cond.dump(); System.out.println(); Stat.indent++; stat.dump(); Stat.indent--; }
}

class IfElse extends Stat {
	Stat ifPart;
	Stat elsePart;
	IfElse(Stat i, Stat e) { ifPart = i; elsePart = e; }
	void dump() { ifPart.dump(); super.dump(); System.out.println("else "); Stat.indent++; elsePart.dump(); Stat.indent--; }
}

class While extends Stat {
	Expr cond;
	Stat stat;
	While(Expr e, Stat s) { cond = e; stat = s; }
	void dump() { super.dump(); System.out.print("while "); cond.dump(); System.out.println(); Stat.indent++; stat.dump(); Stat.indent--; }
}

class Read extends Stat {
	Obj obj;
	Read(Obj o) { obj = o; }
	void dump() { super.dump(); System.out.println("read " + obj.name); }
}

class Write extends Stat {
	Expr e;
	Write(Expr x) { e = x; }
	void dump() { super.dump(); System.out.print("write "); e.dump(); System.out.println(); }
}

class Block extends Stat {
	List<Stat> stats = new ArrayList<Stat>();
	void add(Stat s) { stats.add(s); }
	void dump() {
		super.dump();
		System.out.println("Block("); Stat.indent++;
		for (Stat s: stats) { s.dump(); }
		Stat.indent--; super.dump(); System.out.println(")");
	}
}
