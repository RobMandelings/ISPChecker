import * as ModuleUtil from "./Module"

function parseModule(jsonModule: Object): ModuleUtil.Module {
    const commonFields = jsonModule["commonFields"];
    return new ModuleUtil.Module(
        commonFields.name,
        commonFields.description,
        commonFields.courses,
        parseConstraints(jsonModule.constraints),
        jsonModule.subModules.map(parseModule),
        true);
}