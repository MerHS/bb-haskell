import random

charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

# generate random line with restricted character set
def gen_line(n, char_len):
    return "".join([charset[random.randint(0, char_len - 1)] for _ in range(0, n)])


def gen_test_str(m, n, char_len):
    lines = [f"{m} {n}"]

    patterns = [gen_line(m, char_len) for _ in range(m)]
    texts = [gen_line(n, char_len) for _ in range(n)]

    # embed patterns in text for min, max (3, n/m) times
    embed_cnt_max = max(3, n // m)
    embed_cnt = random.randint(3, embed_cnt_max)

    for _ in range(embed_cnt):
        x = random.randint(0, n - m)
        y = random.randint(0, n - m)

        for pat_i, text_i in enumerate(range(y, y + m)):
            txt = texts[text_i]
            new_text_line = txt[0:x] + patterns[pat_i] + txt[x + m :]
            texts[text_i] = new_text_line

    return "\n".join(lines + patterns + texts)


def main():
    char_lens = [5, 25, 50, 62]
    m_list = [5, 10, 20, 50, 100]
    n_list = [20, 50, 100, 200, 1000]

    for iter in range(10):
        for char_len in char_lens:
            print(f'{iter}-{char_len}/10')
            for (m_idx, m) in enumerate(m_list):
                for n in n_list[m_idx:]:
                    test_str = gen_test_str(m, n, char_len)
                    with open(f'bench/data/{m}_{n}_{char_len}_{iter}.txt', 'w') as f:
                        f.write(test_str)

if __name__ == "__main__":
    main()
