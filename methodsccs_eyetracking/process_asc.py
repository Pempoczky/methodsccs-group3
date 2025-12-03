import re
from pathlib import Path

def rewrite_trial_markers(infile, outfile):
    """
    Read infile (.asc), find Start of trial, var trial <idx>, End of trial occurrences.
    For each var trial <idx>:
      - change the nearest preceding unused "Start of trial" line to:
          MSG\t<timestamp> var StartTrial <idx>
      - change the nearest following unused "End of trial" line to:
          MSG\t<timestamp> var EndTrial <idx>

    Writes modified content to outfile.
    """

    # Read all lines
    lines = Path(infile).read_text(encoding="utf-8").splitlines(keepends=True)

    # Regexes (allow variable whitespace and optional tabs)
    start_re = re.compile(r"^(MSG\s+(\d+)\s+)Start of trial\s*$", re.IGNORECASE)
    trial_re = re.compile(r"^(MSG\s+(\d+)\s+)var\s+trial\s+(\d+)\s*$", re.IGNORECASE)
    end_re   = re.compile(r"^(MSG\s+(\d+)\s+)End of trial\s*$", re.IGNORECASE)

    # Collect positions
    starts = []   # list of (line_index, timestamp_prefix, full_line)
    trials = []   # list of (line_index, timestamp_prefix, trial_idx, full_line)
    ends   = []   # list of (line_index, timestamp_prefix, full_line)

    for i, ln in enumerate(lines):
        m = start_re.match(ln)
        if m:
            starts.append((i, m.group(1), ln))
            continue
        m = trial_re.match(ln)
        if m:
            trials.append((i, m.group(1), int(m.group(3)), ln))
            continue
        m = end_re.match(ln)
        if m:
            ends.append((i, m.group(1), ln))
            continue

    # Mark used starts/ends
    used_start_idx = set()
    used_end_idx = set()

    # For quick search, we will use the positions arrays sorted by line index (they already are)
    # Convert to lists of just indices to compare easily
    start_positions = [s[0] for s in starts]
    end_positions   = [e[0] for e in ends]

    # We'll build a copy of lines we can modify
    out_lines = list(lines)

    # Helper to find last unused start before a position
    def find_prior_unused_start(pos):
        # iterate starts in reverse to find the first with index < pos and unused
        for si, (line_idx, ts_prefix, full) in reversed(list(enumerate(starts))):
            if line_idx < pos and si not in used_start_idx:
                return si, starts[si]
        return None, None

    # Helper to find first unused end after a position
    def find_next_unused_end(pos):
        for ei, (line_idx, ts_prefix, full) in enumerate(ends):
            if line_idx > pos and ei not in used_end_idx:
                return ei, ends[ei]
        return None, None

    # Process each var trial in order
    for trial_entry_index, (trial_line_idx, trial_ts_prefix, trial_idx, trial_full) in enumerate(trials):
        # 1) find nearest preceding unused Start of trial
        si, start_tuple = find_prior_unused_start(trial_line_idx)
        if start_tuple is None:
            print(f"Warning: no unused 'Start of trial' found before 'var trial {trial_idx}' at line {trial_line_idx+1}")
        else:
            start_line_idx, start_ts_prefix, start_full = start_tuple
            # Replace the start line preserving timestamp prefix
            new_start = f"{start_ts_prefix}var StartTrial {trial_idx}\n"
            out_lines[start_line_idx] = new_start
            used_start_idx.add(si)

        # 2) find nearest following unused End of trial
        ei, end_tuple = find_next_unused_end(trial_line_idx)
        if end_tuple is None:
            print(f"Warning: no unused 'End of trial' found after 'var trial {trial_idx}' at line {trial_line_idx+1}")
        else:
            end_line_idx, end_ts_prefix, end_full = end_tuple
            new_end = f"{end_ts_prefix}var EndTrial {trial_idx}\n"
            out_lines[end_line_idx] = new_end
            used_end_idx.add(ei)

    # Optionally: report starts or ends left unmatched (not necessary but helpful)
    unused_starts = [starts[i][0]+1 for i in range(len(starts)) if i not in used_start_idx]
    unused_ends = [ends[i][0]+1 for i in range(len(ends)) if i not in used_end_idx]
    if unused_starts:
        print(f"Note: {len(unused_starts)} 'Start of trial' lines were not matched to any 'var trial' (line numbers): {unused_starts}")
    if unused_ends:
        print(f"Note: {len(unused_ends)} 'End of trial' lines were not matched to any 'var trial' (line numbers): {unused_ends}")

    # Write output
    Path(outfile).write_text("".join(out_lines), encoding="utf-8")
    print(f"Wrote modified file to: {outfile}")

# Example usage
if __name__ == "__main__":
    rewrite_trial_markers("sub_3_copy.asc", "sub3_modified.asc")
