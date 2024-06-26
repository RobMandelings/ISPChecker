import {Module, ModuleConstraint} from "./Structs"
import * as Constraints from "./Constraints";
import {Constraint} from "./Constraints";

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

export function parseModuleConstraint(json: any): ModuleConstraint {
    return {
        description: json.contents[0],
        constraint: parseConstraint(json.contents[1])
    };
}

export function parseConstraint(json: any): Constraints.Constraint {

    if (json.tag === "MaxSPConstraint") {
        return new Constraints.MaxSPConstraint(json.contents[0]);
    } else if (json.tag === "MinSPConstraint") {
        return new Constraints.MinSPConstraint(json.contents[0]);
    } else if (json.tag === "IncludedCourseConstraint") {
        return new Constraints.IncludedCourseConstraint(json.contents[0]);
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
        return new Constraints.NotConstraint(parseConstraint(json.contents[0]));
    } else {
        throw new Error("Unknown constraint type: " + json.tag);
    }
}