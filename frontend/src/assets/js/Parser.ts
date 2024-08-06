import {ConstraintResult, Course, Module, ModuleConstraint, ModuleConstraintResult} from "./Structs"
import * as Constraints from "./Constraints";
import {Constraint} from "./Constraints";

function parseConstraintResult(json: any): ConstraintResult {
    const failed = json.tag === "ConstraintFail"
    const errorMsg = failed ? json.errorMsg : null;
    const subResults = failed ? json.subResults.map(parseConstraintResult) : null;

    return {
        failed: failed,
        errorMsg: errorMsg,
        subResults: subResults
    }
}

export function parseModuleConstraintResult(json: any): ModuleConstraintResult {
    const failed = json.tag === "ModuleFail"
    const constraintResults = failed ? json.constraintResults.map(parseConstraintResult) : null;
    const subModuleResults = failed ? json.subModuleResults.map(parseModuleConstraintResult) : null
    return {
        failed: failed,
        constraintResults: constraintResults,
        subModuleResults: subModuleResults
    }
}

export function parseModule(json: any): Module {

    return {
        name: json.commonFields.name,
        desc: json.commonFields.description,
        courseCodes: json.commonFields.courses,
        moduleConstraints: json.commonFields.constraints.map(parseModuleConstraint),
        subModules: json.subModules.map(parseModule),
        isActive: false
    };
}

export function parseCourse(json: any): Course {
    let semester = null;
    switch (json.period) {
        case "FirstSem":
            semester = 1;
            break;
        case "SecondSem":
            semester = 2;
            break;
        case "AllYear":
            semester = 3;
            break;
    }
    return {
        code: json.code,
        name: json.name,
        description: json.description,
        studyPoints: json.studyPoints,
        semester: semester
    }
}

export function parseModuleConstraint(json: any): ModuleConstraint {
    return {
        description: json.contents[0],
        constraint: parseConstraint(json.contents[1])
    };
}

export function parseConstraint(json: any): Constraints.Constraint {

    if (json.tag === "MaxSPConstraint") {
        return new Constraints.MaxSPConstraint(json.contents);
    } else if (json.tag === "MinSPConstraint") {
        return new Constraints.MinSPConstraint(json.contents);
    } else if (json.tag === "IncludedCourseConstraint") {
        return new Constraints.IncludedCourseConstraint(json.contents);
    } else if (["OrConstraint", "NorConstraint", "XorConstraint", "AndConstraint", "NandConstraint"].includes(json.tag)) {
        let type = null;
        switch (json.tag) {
            case "OrConstraint":
                type = Constraints.OrConstraint;
                break;
            case "NorConstraint":
                type = Constraints.NorConstraint;
                break;
            case "XorConstraint":
                type = Constraints.XorConstraint;
                break;
            case "AndConstraint":
                type = Constraints.AndConstraint;
                break;
            case "NandConstraint":
                type = Constraints.NandConstraint;
                break;
            default:
                throw new Error("Unknown constraint type: " + json.tag);
        }
        return new type(...json.contents.map(parseConstraint));

    } else if (json.tag === "notConstraint") {
        return new Constraints.NotConstraint(parseConstraint(json.contents));
    } else {
        throw new Error("Unknown constraint type: " + json.tag);
    }
}