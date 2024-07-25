import glob
import re
import subprocess
import sys

exe_name = "tests/prog"


def csi(s, n):
    return f"\033[{n}m{s}\033[0m"


subprocess.run(["dune", "build"])


for file in glob.glob("tests/*.tig", recursive=True):
    with open(file, "r") as stream:
        source = stream.read()

        expected_stdout = ""
        for match in re.finditer(r"// out: (.*)\n", source):
            expected_stdout += match.group(1)

        subprocess.run(["./_build/default/bin/main.exe", file, "-o", exe_name + ".s"])
        subprocess.run(
            ["gcc", "-no-pie", "-o", exe_name, "runtime.c", exe_name + ".s"],
        )
        result = subprocess.run(
            ["./" + exe_name],
            capture_output=True,
            text=True,
        )

        print(f"### {file}: ", end="")
        if expected_stdout != result.stdout:
            print(csi(csi("WA", 31), 1))

            def pretty_print(o, e):
                print(csi("produced:", 4))
                print(o, end="")
                print(csi("expected:", 4))
                print(e, end="")

            if expected_stdout != result.stdout:
                pretty_print(result.stdout, expected_stdout)
        else:
            print(csi(csi("OK", 32), 1))
