import pickle

import sys
def read_fasta(filename):
    with open(filename, 'r') as file:
        # Skip the description line
        file.readline()
        sequence = file.read().replace('\n', '')
    return sequence

def calculate_score(s1, s2, l1, l2, startpoint):
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]:
                score += 1
    return score


def find_best_alignments(s1, s2):
    l1, l2 = len(s1), len(s2)
    best_score = -1
    best_alignments = []

    for i in range(l1):
        score = calculate_score(s1, s2, l1, l2, i)
        if score > best_score:
            best_score = score
            best_alignments = [("." * i + s2, s1, score)]
        elif score == best_score:
            best_alignments.append(("." * i + s2, s1, score))

    return best_alignments

def save_alignments(filename, alignments):
    with open(filename, 'wb') as file:
        pickle.dump(alignments, file)

def main():
    if len(sys.argv) > 2:
        seq1 = read_fasta(sys.argv[1])
        seq2 = read_fasta(sys.argv[2])
    else:
        # Use default sequences
        seq1 = read_fasta('data/407228326.fasta')
        seq2 = read_fasta('data/407228412.fasta')

    s1, s2 = (seq1, seq2) if len(seq1) >= len(seq2) else (seq2, seq1)
    best_alignments = find_best_alignments(s1, s2)

    save_alignments('../results/alignment_results.pkl', best_alignments)
    print(f"Saved {len(best_alignments)} best alignments to 'results/alignment_results.pkl'")

if __name__ == "__main__":
    main()
