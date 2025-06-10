# Structural Topic Models for Open-Ended Survey Responses: An Extension

This repository serves the replication files and outputs of a replication of research conducted by Roberts et. al, **Structural Topic Models for Open-Ended Survey Responses**. The corresponding files for reference are accessible at [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/29405). The supplementary appendix which is utilized in the report and for code replication is linked [here](https://scholar.harvard.edu/files/dtingley/files/ajpsappendix.pdf).


## Motivation 

The motivation for this project extension is to evaluate how modern frameworks, namely transformer architectures, hold-up with STM's - how do they differ and where do they converge. 

# Project Structure

The project structure is shown below. We outline the function of each subdirectory below. 

```
├── docs
│   ├── 01
│   ├── 02
│   └── 03
├── output
│   ├── anes
│   ├── ext-anes
│   ├── ext-gadarian
│   ├── gadarian
│   ├── rand
│   └── validation
├── README.md
├── scripts
│   ├── anes.R
│   ├── extension.ipynb
│   ├── gadarian.R
│   ├── rand.R
│   └── validation.R
└── structure.txt 
```

* `docs`: Houses three subdirectories corresponding to rmarkdown and pdf files of my project proposal, proposal presentation, and final report chronologically ordered.
* `output`: Contains txt and visualization outputs from my replication and extension code.
* `scripts`: R and Python scripts of all replication and extension processes. 

## Project Workflow 

An (incredibly) simplified project workflow is shown below. 

1. Environment is setup for the data workflow
2. Replication code is defined and results are interpreted 
3. Extension code is defined and results are interpreted 
4. Report write-up is written and results (from prev. parts) are conglomerated
5. Video presentation is recorded and presented 

## Extension Methodology 

For this extension, we developed a pipeline for extracting and analyzing latent topics in open-ended survey data using BERTopic. We conduct a post hoc covariate analysis in this setting 
to analyze topic prevalence similarly to the STM model. 

In the form of mathematical formulaic, let:

* $D = \{d_1, \dots, d_N\}$: a corpus of open-ended text responses
* $X_i$: metadata for respondent i (e.g., treatment, ideology, demographics)

The pipeline proceeds as follows:

1. Sentence embedding: each document $d_i$ is mapped to a semantic vector:

$$
e_i = \text{Encoder}(d_i) \in \mathbb{R}^p
$$

We use a pretrained transformer-based encoder such as "all-MiniLM-L6-v2".

2. Dimensionality reduction: embeddings are projected into a low-dimensional latent space to preserve semantic similarity while enabling efficient clustering: 

$$
z_i = \text{UMAP}(e_i), \quad z_i \in \mathbb{R}^k
$$

with $k \ll p$, typically 2–5 dimensions.


3. Topic clustering: documents are then clustered using an unsupervised clustering algorithm (e.g KMeans & HDBSCAN) to assign topic labels:


$$
\text{topic}_i = \text{Clustering-Algorithm}(z_i), \quad \text{topic}_i \in \{1, \dots, K\}
$$

where $K$ is the number of topics chosen. 

4. Topic Representation: each topic $T_j$ is denoted by a ranked list of informative terms extracted via TF-IDF weighting across documents for that topic:

$$
T_j = \{w_{j1}, w_{j2}, \dots, w_{jn}\}
$$

where $w_{jk}$ are the top n words ranked by importance to topic $j$.


5. Covariate Modeling: for a selected topic $t^*$, define a binary indicator:

$$
y_i = \mathbb{I}(\text{topic}_i = t^*)
$$

We model the relationship between topic assignment and respondent covariates using a generalized linear model (in our case, a logistic model):

$$
\log \left( \frac{\Pr(y_i = 1)}{1 - \Pr(y_i = 1)} \right) = \beta_0 + \beta^\top X_i + \gamma^\top (X_i \odot Z_i)
$$

where $X_i$ contains main effects and $X_i \odot Z_i$ includes optional interactions (e.g., treatment × ideology).

From the fitted model, proceed to generate predicted topic assignment probabilities across a grid of covariate values:

$$
\hat{p}_i = \Pr(y_i = 1 \mid X_i)
$$
