# Jai Dog Rescue KAP survey analysis

This repository contains the Python notebooks supporting the knowledge,
attitudes and practices (KAP) survey analyses used as supplementary evidence for
the manuscript *The Impacts of Dog Sterilization Campaigns in Thailand:
Understanding Changes in Dog Population Dynamics*.

The district comparisons presented here are exploratory and should not be
interpreted causally.

## Contents

- `01_roaming_dogs.ipynb` — questions about roaming dogs and their management
- `02_dog_ownership.ipynb` — dog ownership and household dog movement
- `03_part_c.ipynb` — dog-level survey analysis
- `04_part_d.ipynb` — bite and management analysis

The notebooks contain code only. Their saved outputs and execution counts have
been removed.

## Data availability

The participant-level survey workbook is not publicly available because it
contains confidential survey participant data. Access is limited to the study
and manuscript team. Workbook and column names appearing in the code describe
the analysis structure; they do not contain participant answers. Exact results
require authorized access to the restricted workbook.

Members of the study and manuscript team should keep the workbook outside this
repository and set its location for the current Command Prompt session:

```cmd
set "JAI_SURVEY_DATA=D:\secure-research-data\survey.xlsx"
```

Do not copy the real workbook into this repository.

## Running the notebooks

The analysis environment used Python 3.11.3.

```cmd
python -m venv .venv
.venv\Scripts\activate.bat
pip install -r requirements.txt
```

Open the notebooks in VS Code with the Jupyter extension and run them in
numerical order.

Before committing future changes, clear all notebook outputs and check
`git status` carefully. Do not commit survey files, generated HTML, or
unreviewed figures and tables.

## License

No reuse license has been selected. The manuscript authors or responsible
institution should choose one before public release.

