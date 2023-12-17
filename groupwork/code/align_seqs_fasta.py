
import sys

def read_fasta(filename):
    '''Reads a fasta file and returns the sequence'''
    with open(filename, 'r') as file:
        # Skip the first line (header)
        next(file)
        # Read and concatenate the sequence lines
        sequence = ''.join(line.strip() for line in file)
    return sequence

def calculate_score(s1, s2, l1, l2, startpoint):
    matched = ""
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]:
                matched += "*"
                score += 1
            else:
                matched += "-"

    return score, matched

def align_sequences(seq1, seq2):
    l1, l2 = len(seq1), len(seq2)
    if l1 >= l2:
        s1, s2 = seq1, seq2
    else:
        s1, s2 = seq2, seq1
        l1, l2 = l2, l1

    my_best_align = None
    my_best_score = -1
    my_best_matched = ""

    for i in range(l1):
        score, matched = calculate_score(s1, s2, l1, l2, i)
        if score > my_best_score:
            my_best_align = "." * i + s2
            my_best_score = score
            my_best_matched = "." * i + matched

    return my_best_align, s1, my_best_matched, my_best_score

if __name__ == "__main__":
    # Default files if no input is given
    default_files = ['407228326.fasta', '407228412.fasta']

    # Check if file names are provided as command line arguments
    input_files = sys.argv[1:] if len(sys.argv) > 1 else default_files

    # Read sequences from the files
    seq1 = read_fasta(input_files[0])
    seq2 = read_fasta(input_files[1])

    # Align the sequences
    best_align, s1, matched, best_score = align_sequences(seq1, seq2)

    # Print the result
    print(matched)
    print(best_align)
    print(s1)
    print("Best score:", best_score)
