import {Module} from "./Structs"

function parseModule(jsonModule: Object): Module {
    const commonFields = jsonModule["commonFields"];
    return {
        name: commonFields.name,
        desc: commonFields.description,
        courseCodes: commonFields.courses,
        constraints: parseConstraints(jsonModule.constraints),
        subModules: jsonModule.subModules.map(parseModule),
        isActive: true
    };
}