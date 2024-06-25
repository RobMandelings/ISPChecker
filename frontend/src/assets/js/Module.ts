// I chose not to have the individual constraints as well because this would make it much harder to maintain
export interface ModuleJSON {
    name: string;
    desc: string;
    courseCodes: string[];
    constraints: string[];
    subModules: ModuleJSON[];
    isActive: boolean;
}