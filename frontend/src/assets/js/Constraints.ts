export abstract class ConstraintVisitor {

    abstract visitAndConstraint(c: AndConstraint): any;

    abstract visitOrConstraint(c: OrConstraint): any;

    abstract visitNorConstraint(c: NorConstraint): any;

    abstract visitNandConstraint(c: NandConstraint): any;

    abstract visitXorConstraint(c: XorConstraint): any;

    abstract visitNotConstraint(c: Constraint): any;

    abstract visitMinSPConstraint(c: MinSPConstraint): any;

    abstract visitMaxSPConstraint(c: MaxSPConstraint): any;

    abstract visitRemainingSPConstraint(c: RemainingSPConstraint): any;

    abstract visitSameYearConstraint(c: SameYearConstraint): any;

    abstract visitScopedConstraint(c: ScopedConstraint): any;

    abstract visitIncludedCourseConstraint(c: IncludedCourseConstraint): any;

}

export abstract class Constraint {
    public abstract accept(visitor: ConstraintVisitor): any;
}

export class NotConstraint extends Constraint {
    public c: Constraint;

    constructor(c: Constraint) {
        super();
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitNotConstraint(this.c);
    }
}

export abstract class BinaryConstraint extends Constraint {
    public lhs: Constraint;
    public rhs: Constraint;

    constructor(lhs: Constraint, rhs: Constraint) {
        super();
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

export class AndConstraint extends BinaryConstraint {

    accept(visitor: ConstraintVisitor): any {
        visitor.visitAndConstraint(this);
    }
}

export class XorConstraint extends BinaryConstraint {

    accept(visitor: ConstraintVisitor): any {
        visitor.visitXorConstraint(this);
    }
}

export class OrConstraint extends BinaryConstraint {

    accept(visitor: ConstraintVisitor): any {
        visitor.visitOrConstraint(this);
    }
}

export class NorConstraint extends BinaryConstraint {

    accept(visitor: ConstraintVisitor): any {
        visitor.visitNorConstraint(this);
    }
}

export class NandConstraint extends BinaryConstraint {

    accept(visitor: ConstraintVisitor): any {
        visitor.visitNandConstraint(this);
    }
}

export class MinSPConstraint extends Constraint {
    public sp: number;

    constructor(minSP: number) {
        super();
        this.sp = minSP;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitMinSPConstraint(this)
    }
}

export class MaxSPConstraint extends Constraint {
    public sp: number;

    constructor(maxSP: number) {
        super();
        this.sp = maxSP;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitMaxSPConstraint(this);
    }
}

export class RemainingSPConstraint extends Constraint {
    public sp: number;

    constructor(remainingSP: number) {
        super();
        this.sp = remainingSP;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitRemainingSPConstraint(this);
    }
}

export class SameYearConstraint extends Constraint {
    public courseCode1: string;
    public courseCode2: string;

    constructor(courseCode1: string, courseCode2: string) {
        super();
        this.courseCode1 = courseCode1;
        this.courseCode2 = courseCode2;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitSameYearConstraint(this);
    }
}

export class ScopedConstraint extends Constraint {
    public scope: string[];
    public constraint: Constraint;

    constructor(scope: string[], constraint: Constraint) {
        super();
        this.scope = scope;
        this.constraint = constraint;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitScopedConstraint(this);
    }
}

export class IncludedCourseConstraint extends Constraint {
    public courseCode: string;

    constructor(courseCode: string) {
        super();
        this.courseCode = courseCode;
    }

    accept(visitor: ConstraintVisitor): any {
        return visitor.visitIncludedCourseConstraint(this);
    }
}