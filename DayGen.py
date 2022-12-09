import os

days = [f"Day{num:02d}" for num in range(1,26)]
print(",\n".join(f"({d}.part1, {d}.part2)" for d in  days))

print("\n")
print("\n".join(f"import {d}.Ex as {d} (part1, part2)" for d in days))

print("\n")
print("\n".join(f"    {d}.Ex" for d in days))


ex_base_file = """
module {0}.Ex (part1, part2) where

part1 _ = ""
part2 _ = ""
"""[1:]

for day in days:
    folder = os.path.join("app", day)
    if not os.path.exists(os.path.join("app", day)):
        os.mkdir(folder)
        open(os.path.join(folder, "Ex.hs"), 'w').write(ex_base_file.format(day))


