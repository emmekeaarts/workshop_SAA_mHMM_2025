# Uncovering Personalized Dynamics with Multilevel Hidden Markov Models
This webpage contains all the materials for an afternoon workshop on Uncovering Personalized Dynamics with Multilevel Hidden Markov Models. The materials on this website are [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) licensed.

![cc](https://mirrors.creativecommons.org/presskit/icons/cc.svg) ![by](https://mirrors.creativecommons.org/presskit/icons/by.svg)


## Course objectives

The goal of this workshop is to provide a gentle, hands-on introduction to multilevel hidden Markov models. 

Facilitated by widespread availability of mobile devices and improvements in dedicated designs such as Ecological Momentary Assessment (EMA), the collection of intensive longitudinal data has become increasingly popular.  The data collected using such designs allow us to study dynamics of psychological processes at an unprecedented temporal granularity. To uncover and adequately summarize systematic dynamics in these data, we require fitting statistical models. Common approaches in this area are to study the autoregression and cross-lagged regression using methods such as autoregressive (AR) models, Vector Autoregressive (VAR) models, and dynamic structural equation models (DSEM). 

When modelling a process in which we expect subjects to switch between multiple psychological discrete states, a different approach might be more informative. Discrete states could, for example, be manic and depressive states in bipolar disorder or recovery and relapse states as seen in addiction. In most settings, we do not directly observe the state, but only the subjective experiences associated with the states. The multilevel hidden Markov model (HMM) is a promising novel approach to translate information on these latent switching dynamics into a statistical model. That is, in contrast to e.g., commonly used VAR based models, the hidden Markov model quantifies dynamics on processes where subjects are allowed to switch between multiple psychological states. By extending the HMM to the multilevel framework, we can obtain a description of subject-specific process dynamics and formally quantify heterogeneity across subjects.

First, we give an accessible introduction on using HMMs to model EMA data. The concepts and benefits the multilevel HMM will be explained in an intuitive manner using an empirical application on (suicidal) crisis dynamics. The introduction is followed by a hands-on, easy-to-follow workshop on fitting, performing model selection, interpreting parameters, and visualizing results of the multilevel hidden Markov model. During the hands-on part, we will analyze an open-source empirical EMA dataset with a user-friendly and open-source application. After the workshop, you will have added a new method to your statistical toolbox to extract personalized latent dynamics from intense longitudinal data. 

At the end of this session, participants have a firm grasp of the basics of the multilevel hidden Markov model, as well as the skills to start applying this method in their own work.

## Workshop instructors 

Your workshop instructors are [dr. Emmeke Aarts](https://www.uu.nl/staff/EAarts), [dr. Jonas Haslbeck](https://jonashaslbeck.com), and [Pepijn Vink](https://www.uu.nl/staff/PAVink). 

## Prerequisites

The workshop is open to researchers from all areas within the ambulatory assessment field and to researchers at all career stages. 

Please bring your laptop. To work with the workshop materials, please load the full documentation package `workshop_SAA_mHMM_2025` as a `.zip` file (near the top of this webpage, under the green `Code` button, you can find the option `Download ZIP`). 

We assume the following:

- You are comfortable with estimating and interpreting univariate and multivariate statistical models such as regression models.
- You are familiar with the `R` programming language and you have a recent version installed.
- It's a bonus if you are familiar with multilevel modelling and/or somewhat familiar with hidden Markov models.
- You have installed the following `R` packages on your computer:
  - `mHMMbayes` (version 1.1.0)
  - `ggplot2`

You can use the following code to install these at once:
```r
install.packages(c("ggplot2", "mHMMbayes"))
```
  

## Workshop schedule & materials

| Time  | Duration | Activity     | Content                                                         | link |
| :---: | :------: | :----------- | :-------------------------------------------------------------- | :--- |
| 14:00 | 45       | Lecture      | Introduction & multilevel hidden Markov model                   | [`intro.pdf`](./lectures/01_introduction/Intro.html) |
| 14:45 | 60       | Practical    | Fitting a mHMM + group level parameters                         | [`intro.html`](./practicals/01_introduction/Intro_pract.html) |
| 15:45 | 15       | Break        |                                                                 |      |
| 16:00 | 45       | Lecture      | Model selection and fit + subject level parameters  | [`More_advanced.hmtl`](./lectures/02_More_advanced/More_advanced.html) |
| 16:45 | 45       | Practical    | Model selection and fit + subject level parameters  | [`More_advanced_pract.html`](./practicals/02_more_advanced/More_advanced_pract.html) |
| 17:30 | 30       | Conclusion   | Final points + questions                                        |  [`Final_points.html`](./lectures/03_final_points/Final_points.html)    |


## Additional links

- Source paper on hidden markov models: L. R. Rabiner, "A tutorial on hidden Markov models and selected applications in speech recognition," in *Proceedings of the IEEE*, vol. 77, no. 2, pp. 257-286, Feb. 1989. Doi: 10.1109/5.18626.  [link](https://doi.org/10.1109/5.18626)
- Introductory book on hidden Markov models in R:  Hidden Markov Models for Time Series: An Introduction Using R, by Walter Zucchini, Iain L. Macdonald, and Roland Langrock. Published by CRC Press, 2016.
- For an accessible yet precise introduction to HMMs and their multilevel extension, a numerical evaluation of model selection and parameter estimation performance of the multilevel HMM in situations that resemble typical EMA research designs, and a
fully reproducible tutorial on the multilevel HMM using the R package mHMMbayes, see this [pre-print](https://doi.org/10.31234/osf.io/y2u5s_v2). Aarts, E., & Haslbeck, J. M. B. (2025, April 3). Modelling Psychological Time Series with Multilevel Hidden Markov Models: A Numerical Evaluation and Tutorial. *PsyArXiv* Doi: 10.31234/osf.io/y2u5s_v2.
- Example of multilevel HMM using continuous input data applied to patients with [bipolar disorder](https://osf.io/preprints/psyarxiv/egp82/) (for pre-print see [here](https://osf.io/preprints/psyarxiv/egp82/)), [suicidal crisis](https://doi.org/10.3389/fpsyt.2024.1501911), and individuals on the  [psychosis](https://doi.org/10.1017/S003329172500056X) continuum.
- Example of multilevel HMM using categorical input data applied to [nonverbal communication in patient-therapist dyads](https://doi.org/10.1016/j.jadr.2023.100635).
- Example of multilevel HMM using count input data to extract [neural spiking based behavioural event states](https://doi.org/10.1111/ejn.16065).
- For details on the adopted estimation algorithms to infer the parameters of the multilevel hidden Markov model have a look at the [estimation vignette of the mHMMbayes package](https://cran.r-project.org/web/packages/mHMMbayes/vignettes/estimation-mhmm.pdf).


## Contact

For questions about this course, you can contact the instructor Emmeke ([e.aarts@uu.nl](mailto:e.aarts@uu.nl)) directly. 




    

