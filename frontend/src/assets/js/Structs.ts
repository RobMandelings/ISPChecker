import {Constraint} from "./Constraints";

// I chose not to have the individual constraints as well because this would make it much harder to maintain
export interface Module {
    name: string;
    desc: string;
    courseCodes: string[];
    moduleConstraints: ModuleConstraint[];
    subModules: Module[];
    isActive: boolean;
}

/**
 * This is an example of a json object that can be parsed into a module
 * {
 *   "abc": {
 *     "commonFields": {
 *       "name": "Verdere optie",
 *       "description": "Hellowkes",
 *       "courses": [
 *         "H04IOA"
 *       ],
 *       "constraints": [
 *         {
 *           "tag": "ModuleConstraint",
 *           "contents": [
 *             "Maximaal 1 studiepunt",
 *             {
 *               "tag": "MaxSPConstraint",
 *               "contents": 1
 *             }
 *           ]
 *         }
 *       ],
 *       "activator": "\"\u003CModuleActivator\u003E\""
 *     },
 *     "subModules": []
 *   },
 *   "hoi": {
 *     "commonFields": {
 *       "name": "Subonderdeel",
 *       "description": "Cool",
 *       "courses": [],
 *       "constraints": [],
 *       "activator": "\"\u003CModuleActivator\u003E\""
 *     },
 *     "subModules": []
 *   }
 * }
 * @param json
 */

/** Not an actual constraint, just a constraint with a description attached */
export interface ModuleConstraint {
    description: string;
    constraint: Constraint;
}

export interface ISP {
    courseSelection: {
        passed: string[],
        planned: string[][]
    };
    studyProgram: string;
    options: { [key: string]: string };
}

enum Semester {
    First = 1,
    Second = 2,
    AllYear = 3
}

export interface Course {
    code: string;
    name: string;
    description: string;
    studyPoints: number;
    semester: Semester;
}

export interface ConstraintResult {
    failed: boolean,
    errorMsg: string,
    subResults: ConstraintResult[]
}

export interface ModuleConstraintResult {
    failed: boolean,
    constraintResults: ConstraintResult[]
    subModuleResults: ModuleConstraintResult[]
}