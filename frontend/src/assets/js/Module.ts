// I chose not to have the individual constraints as well because this would make it much harder to maintain

export class ModuleConstraint {
    private desc: string;
}

export class Module {
    public name: string;
    public desc: string;
    public courseCodes: string[];
    public constraints: string[];
    public subModules: Module[];
    public isActive: boolean;


    constructor(name: string, desc: string, courseCodes: string[], constraints: string[], subModules: Module[], isActive: boolean) {
        this.name = name;
        this.desc = desc;
        this.courseCodes = courseCodes;
        this.constraints = constraints;
        this.subModules = subModules;
        this.isActive = isActive;
    }
}