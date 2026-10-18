# Residual Human States

Code for the secondary analyses in

> Herrera-Poyatos, D., Peláez-González, C., Zuheros, C., & Herrera, F. (2026).
> *What Human–AI Interaction Leaves Behind: Residual Human States—A Trajectory-Based
> Framework and Multi-Study Analysis.* Manuscript submitted for publication.

The repository covers Studies 2, 3 and 4 of the paper. Study 1 (Vicente et al., 2025)
uses only published aggregate results, so there is no script for it.

No participant data are included. `get_data.sh` downloads the original data from the
sources below, and `run_all.sh` reproduces the tables and numbers reported in the paper.

## Data

| Study | Original data | Script |
|---|---|---|
| 2 | Cabitza & Vicente (2026): [Google Drive folder](https://drive.google.com/drive/folders/1E-4MMbgPMU1t2jz7bWK9Gu5drc_XhRLF), file `Data/MachineMentoring_DataHD.csv` | `P2_cabitza/P2_policy_contrasts.R` |
| 3 | Bastani et al. (2025): [GitHub repository](https://github.com/obastani/GenAICanHarmLearning), commit `2f63dae`, file `main_regressions/final_data.csv` | `P3_bastani/P3_cluster_robust.R` |
| 4 | Corbett & Tangen (2026): [OSF project](https://osf.io/wseq3/?view_only=bba17d9c74ca4dfca02a716cb2ed21f6), *Data* component, `csvs.zip` | `P4_corbett/P4_longitudinal.R` |

If you use these data, please cite both the original article and the dataset.

**Study 2**

- Cabitza, F., & Vicente, L. (2026). Machine learning systems as mentors in human
  learning: The role of AI output in diagnostic knowledge acquisition. *International
  Journal of Human-Computer Studies, 215*, 103855.
  https://doi.org/10.1016/j.ijhcs.2026.103855
- Cabitza, F., & Vicente, L. (2026). Open data for "Machine learning systems as mentors
  in human learning: The role of AI output in diagnostic knowledge acquisition"
  [Data set]. https://drive.google.com/drive/folders/1E-4MMbgPMU1t2jz7bWK9Gu5drc_XhRLF

**Study 3**

- Bastani, H., Bastani, O., Sungu, A., Ge, H., Kabakcı, Ö., & Mariman, R. (2025).
  Generative AI without guardrails can harm learning: Evidence from high school
  mathematics. *Proceedings of the National Academy of Sciences, 122*(26), e2422633122.
  https://doi.org/10.1073/pnas.2422633122
- Bastani, H., Bastani, O., Sungu, A., Ge, H., Kabakcı, Ö., & Mariman, R. (2025). Data
  and code for "Generative AI without guardrails can harm learning: Evidence from high
  school mathematics" [Data set and code]. GitHub.
  https://github.com/obastani/GenAICanHarmLearning

**Study 4**

- Corbett, B. J., & Tangen, J. M. (2026). AI tutors vs. tenacious myths: Evidence from
  personalised dialogue interventions in education. *Computers in Human Behavior, 175*,
  108828. https://doi.org/10.1016/j.chb.2025.108828
- Corbett, B. J., & Tangen, J. M. (2026). Open data and analysis materials for "AI tutors
  vs. tenacious myths: Evidence from personalised dialogue interventions in education"
  [Data set]. OSF. https://osf.io/wseq3/?view_only=bba17d9c74ca4dfca02a716cb2ed21f6

Study 1 is Vicente, L., Matute, H., Fregosi, C., & Cabitza, F. (2025). Machine learning
systems as mentors in human learning: A user study on machine bias transmission in
medical training. *International Journal of Human-Computer Studies, 198*, 103474.
https://doi.org/10.1016/j.ijhcs.2025.103474 (data: https://osf.io/uxy79/). Its data
are not used here.

The data remain under the terms set by their authors.

## Requirements

- Bash (Linux, macOS or WSL), Git, curl, wget, unzip, `sha256sum` and Python 3.
- [`gdown`](https://github.com/wkentaro/gdown) to download the Study 2 folder from Google
  Drive (`pip install gdown`).
- R 4.3 with `readr`, `sandwich`, `clubSandwich`, `dqrng`, `dplyr`, `tidyr`, `nlme`,
  `emmeans`, `afex`, `lme4`, `lmerTest`, `mice`, `car` and `geepack`.
- `fwildclusterboot` 0.14.3, which is no longer on CRAN:

  ```r
  remotes::install_github("s3alfisc/fwildclusterboot", ref = "336bb574")
  ```

The exact package versions used for the reference run are printed at the end of each
log in `expected_output/`.

## Usage

```bash
bash get_data.sh   # downloads to data/
bash run_all.sh    # writes tables to output/P2, output/P3, output/P4 and one log per study
```

Results can be compared with `expected_output/`. The Study 3 bootstrap and the Study 4
multiple imputation take a while. Each study can also be run on its own from the
repository root, e.g. `Rscript P4_corbett/P4_longitudinal.R`.

`get_data.sh` checks the Study 2 file and the Study 4 archive against SHA-256 checksums,
and Study 3 is pinned to a fixed commit, so a change in any source stops the script.
If the Google Drive or OSF download fails, download the folder or component as a zip in
the browser and pass it to the script:

```bash
CABITZA_DATA_ZIP=/path/to/drive.zip CORBETT_DATA_ZIP=/path/to/Data.zip bash get_data.sh
```

To use data stored elsewhere, set `CABITZA_DIR`, `BASTANI_DIR` or `CORBETT_CSV_DIR` when
running the R scripts. `OUT_DIR` changes the output folder.

## Licence

The code is released under the MIT licence (see `LICENSE`). To cite it, use the paper
above or `CITATION.cff`.
