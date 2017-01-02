# Fixing the treebank for Portuguese (by Andre Martins)

Some languages (such as Portuguese) have treebanks that are inconsistent with the other languages regarding treatment of contractions, clitics, etc. For Portuguese, there are a few scripts and manual steps that were necessary to create a consistent version of the treebank. The following steps were followed (instructions are with respect to version 1.3 of the universal treebanks):

1. Concatenate the train/dev/test Portuguese treebanks:
  ```
  cat pt-ud-train.conllu pt-ud-dev.conllu pt-ud-test.conllu > pt-ud-all.conllu
  ```  

2. Extract file with Portuguese contractions hidden in former MWEs. The script below produces a file containing contractions and their annotation statistics. 
  ```
  python extract_portuguese_contraction_annotations.py pt-ud-all.conllu > contraction_statistics.txt
  ```  

3. Manually curate the contraction file (only one annotation per contraction should be produced). 
  ```
  cp contraction_statistics.txt contraction_statistics_curated.txt
  <edit contraction_statistics_curated.txt manually>
  ...
  ```

4. Run the script below for train/dev/test data to split contractions using the curated contractions file. This will split contractions hidden in former MWEs, and additionally, for all contractions (both for MWEs and for the others) it creates additional tokens like "1-2 da _ _ ..." that allow to recover the contracted word if necessary (this is for consistency with the other universal treebanks).
  ```
  python fix_portuguese_treebank.py pt-ud-train.conllu contraction_statistics_curated.txt > pt_v2.0.1-ud-train.conllu
  python fix_portuguese_treebank.py pt-ud-dev.conllu contraction_statistics_curated.txt > pt_v2.0.1-ud-dev.conllu
  python fix_portuguese_treebank.py pt-ud-test.conllu contraction_statistics_curated.txt > pt_v2.0.1-ud-test.conllu
  ```

5. Extract file with Portuguese verbs with enclitics (e.g. "fê-lo"). The script below produces the previous splitting of those verbs, and their replacement according to our tokenizer, for example "fez-|lo fê|-lo". NOTE: this assumes that the number of split words will not change.
  ```
  python extract_portuguese_clitics.py pt_v2.0.1-ud-train.conllu pt_v2.0.1-ud-dev.conllu pt_v2.0.1-ud-test.conllu > portuguese_clitics.txt
  ```

6. Manually curate the clitics file. NOTE: you can adjust the split words, but not their number.
  ```
  cp portuguese_clitics.txt portuguese_clitics_curated.txt
  <edit portuguese_clitics_curated.txt manually>
  ...
  ```

7. Replace the clitic segments by running the script below. NOTE: the script is generic, it can replace any segment of words by another segment, but the segment lengths must be the same and the annotations will be kept as they are.
  ```
  python replace_treebank_segments.py pt_v2.0.1-ud-train.conllu portuguese_clitics_curated.txt > pt_v2.1.1-ud-train.conllu
  python replace_treebank_segments.py pt_v2.0.1-ud-dev.conllu portuguese_clitics_curated.txt > pt_v2.1.1-ud-dev.conllu
  python replace_treebank_segments.py pt_v2.0.1-ud-test.conllu portuguese_clitics_curated.txt > pt_v2.1.1-ud-test.conllu
  ```
