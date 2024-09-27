import subprocess
from itertools import permutations, product


class Predicate:
    def __init__(self, *args):
        self.args = args

    def __repr__(self):
        class_name = self.__class__.__name__
        args_str = ", ".join(map(str, self.args))
        return f"{class_name}({args_str})"


class PointsTo(Predicate):
    def __init__(self, arg1, arg2):
        super().__init__(arg1, arg2)


class New(Predicate):
    def __init__(self, arg1, arg2):
        super().__init__(arg1, arg2)


class Assign(Predicate):
    def __init__(self, arg1, arg2):
        super().__init__(arg1, arg2)


class Load(Predicate):
    def __init__(self, arg1, arg2, arg3):
        super().__init__(arg1, arg2, arg3)


class PointsToField(Predicate):
    def __init__(self, arg1, arg2, arg3):
        super().__init__(arg1, arg2, arg3)


class Store(Predicate):
    def __init__(self, arg1, arg2, arg3):
        super().__init__(arg1, arg2, arg3)


class Rule:
    def __init__(self, head, *body):
        self.head = head
        self.body = body

    def __repr__(self):
        body_str = ", ".join(map(str, self.body))
        return f"{self.head} :- {body_str}."

    def generate_permutations(self):
        body_permutations = permutations(self.body)
        return [Rule(self.head, *perm) for perm in body_permutations]


# Example usage to represent the given rules
rule1 = Rule(PointsTo("x", "y"), New("x", "y"))
rule2 = Rule(PointsTo("x", "z"), Assign("x", "y"), PointsTo("y", "z"))
rule3 = Rule(PointsTo("x", "v"), Load("x", "y", "f"), PointsTo("y", "z"), PointsToField("z", "f", "v"))
rule4 = Rule(PointsToField("t", "f", "v"), Store("r", "f", "s"), PointsTo("r", "t"), PointsTo("s", "v"))

# Generate all permutations for each rule
rule_permutations = [
    rule1.generate_permutations(),
    rule2.generate_permutations(),
    rule3.generate_permutations(),
    rule4.generate_permutations()
]

def magic_sets_transform(exhaustive):
    output = "untitled/src/datalog/permutations/output.dl"
    # cmd = "./untitled/src/datalog/permutations/transform.sh {} {}".format(exhaustive, output)
    # print("Running command " + cmd)

    # subprocess.run(["untitled/src/datalog/permutations.transform.sh", [exhaustive], [output]])
    subprocess.run(["node", "untitled/src/datalog/permutations/permute.js"],)


# Generate all combinations of permutations from each rule
all_programs = list(product(*rule_permutations))

#
for i, program in enumerate(all_programs, start=1):
    print(f"Program {i}:")

    # Clear analysis file
    template_file = open("untitled/src/datalog/permutations/template.txt", "r")
    exhaustivePath = "untitled/src/datalog/permutations/analysis.dl"
    open(exhaustivePath, "w").close()

    # Paste template and analysis into file
    with open("%s" % exhaustivePath, "a") as file:
        file.write(template_file.read())
        for rule in program:
            file.write(rule.__repr__())
            file.write("\n")
        file.write("\n")
        file.write(".decl query(t: token)\n")
        file.write('query(t) :- PointsTo("x1", t).')

        # Perform the analysis transformation
        magic_sets_transform(exhaustivePath)
        exit(1)

# place under the rules each time
# .decl query(t: token)
# // This line is automatically changed by the transform_program script
# query(t) :- pointsTo("x1", t).
