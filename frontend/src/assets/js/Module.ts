import {Constraint} from "./Constraints";

// I chose not to have the individual constraints as well because this would make it much harder to maintain
export interface Module {
    name: string;
    desc: string;
    courseCodes: string[];
    constraints: ModuleConstraint[];
    subModules: Module[];
    isActive: boolean;
}

/** Not an actual constraint, just a constraint with a description attached */
export interface ModuleConstraint {
    description: string;
    constraint: Constraint;
}