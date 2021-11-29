import argparse
from pathlib import Path


def naive_2d_pattern_search(m, n, pattern, text):
    result = []

    # iterate text lines
    for row_idx in range(0, n - m + 1):
        text_line = text[row_idx]
        pat_line = pattern[0]
        candidates = []

        # find first line of pattern from current text line
        # push candidates of column indices
        for col_idx in range(0, n - m + 1):
            if text_line[col_idx : col_idx + m] == pat_line:
                candidates.append(col_idx)

        # iterate next pattern lines
        for pat_idx, pat_rows in enumerate(pattern[1:]):
            new_cand = []
            text_line = text[row_idx + pat_idx + 1]

            # search candidated column indices only
            for col_idx in candidates:
                if text_line[col_idx : col_idx + m] == pat_rows:
                    new_cand.append(col_idx)

            candidates = new_cand

        # push survived candidates to result list
        for col_idx in candidates:
            result.append((row_idx + m - 1, col_idx + m - 1))

    return result


def main(args):
    # text-parsing
    text_in, text_out, text_check = "", "", ""
    if len(args) >= 1:
        text_in = Path(args[0])
        if len(args) >= 2:
            text_out = Path(args[1])
            if len(args) >= 3:
                text_check = args[2]

    if text_in == "" or not text_in.exists():
        print(f"not found input text: {text_in}")
        return

    lines = [line.strip() for line in text_in.read_text().split("\n")]
    [m, n] = [int(i) for i in lines[0].split(" ")]

    pattern = lines[1 : (m + 1)]
    text = lines[(m + 1) :]

    # search text
    result = naive_2d_pattern_search(m, n, pattern, text)

    # print to stdout or file
    if text_out != "" and text_out.exists():
        temp = [
            line.strip().split(" ")
            for line in text_out.read_text().split("\n")
            if line.strip() != ""
        ]

        test_result = [(int(a), int(b)) for [a, b] in temp]

        result_text = "yes" if result == test_result else "no"

        if text_check != "":
            text_check = Path(text_check)
            text_check.write_text(result_text)
        else:
            print(result_text)
    else:
        for (x, y) in result:
            print(f"{x} {y}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("texts", nargs="*")
    args = parser.parse_args()

    main(args.texts)
