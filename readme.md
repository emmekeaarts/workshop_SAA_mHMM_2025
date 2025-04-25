# Extracting personalised latent dynamics using multilevel hidden Markov models
This webpage contains all the materials for an afternoon workshop on extracting personalised latent dynamics using multilevel hidden Markov models. The materials on this website are [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) licensed.

![cc](https://mirrors.creativecommons.org/presskit/icons/cc.svg) ![by](https://mirrors.creativecommons.org/presskit/icons/by.svg)


## Course objectives

Facilitated by technological advances such as smartphones, smart watches, and sensors, it has become relatively easy and affordable to collect data on groups of individuals with a high temporal resolution: intensive longitudinal data (ILD). Due to the high sampling frequency, ILD can uniquely be used to study how psychological, behavioral and physiological processes unfold over time at the within-person level, and between person differences herein. When the dynamics over time of interest can be represented by a latent construct consisting of mutually exclusive categories, the hidden Markov model (HMM; Rabiner, 1989; de Haan-Rietdijk et al., 2017) is a promising novel approach. The HMM is a probabilistic, unsupervised, longitudinal machine learning method which uncovers empirically derived latent (i.e., hidden) states and the dynamics between these latent states over time. Utilising the multilevel framework, heterogeneity between individuals is accommodated, facilitating the study of individual specific dynamics and differences herein.

The workshop starts with a conceptual introduction on the (multilevel) hidden Markov model and how it fits together with ILD using an empirical example. This is followed by a hands-on workshop using the R CRAN package mHMMbayes. 

At the end of this session, participants have a firm grasp of the basics of the multilevel hidden Markov model, as well as the skills to start applying this method in their own work.


## Prerequisites

Please bring your laptops. To work with the workshop materials, please load the full documentation package `workshop_mHMM_march2024` as a `.zip` file (near the top of this webpage, under the green `Code` button, you can find the option `Download ZIP`). 

We assume the following:

- You are comfortable with estimating and interpreting univariate and multivariate statistical models such as regression models.
- You are familiar with the `R` programming language and you have a recent version installed.
- It's a bonus if you are somewhat familiar with hidden Markov models.
- You have installed the following `R` packages on your computer:
  - `mHMMbayes` (version 1.0.0)
  - `ggplot2`

You can use the following code to install these at once:
```r
install.packages(c("ggplot2", "mHMMbayes"))
```
  

## Workshop schedule & materials

| Time  | Duration | Activity     | Content                                                         | link |
| :---: | :------: | :----------- | :-------------------------------------------------------------- | :--- |
| 13:30 | 45       | Lecture      | Introduction & multilevel hidden Markov model                   | [`intro.pdf`](./lectures/01_introduction/Intro.html) |
| 14:15 | 45       | Practical    | Fitting a mHMM + group level parameters                         | [`intro.html`](./practicals/01_introduction/Intro_pract.html) |
| 15:00 | 15       | Break        |                                                                 |      |
| 15:15 | 45       | Lecture      | Model selection and fit + subject level parameters + covariates | [`More_advanced.hmtl`](./lectures/02_More_advanced/More_advanced.html) |
| 16:00 | 45       | Practical    | Model selection and fit + subject level parameters + covariates | [`More_advanced_pract.html`](./practicals/02_more_advanced/More_advanced_pract.html) |
| 16:45 | 15       | Conclusion   | Final points + questions                                        |  [`Final_points.html`](./lectures/03_final_points/Final_points.html)    |

You can download the dataset we will be using from [`here`](https://github.com/jmbh/EmotionTimeSeries/tree/master/DataClean/Rowland2020), see practical 1 for an introduction of the dataset and references. Save it in a nicely accessible place, we will be using it in every practical.


## Additional links

- Source paper on hidden markov models: L. R. Rabiner, "A tutorial on hidden Markov models and selected applications in speech recognition," in Proceedings of the IEEE, vol. 77, no. 2, pp. 257-286, Feb. 1989, doi: 10.1109/5.18626.  [link](https://doi.org/10.1109/5.18626)
- Introductory book on hidden Markov models in R:  Hidden Markov Models for Time Series: An Introduction Using R, by Walter Zucchini, Iain L. Macdonald, and Roland Langrock. Published by CRC Press, 2016.
- [Tutorial vignette mHMMbayes package](https://cran.r-project.org/web/packages/mHMMbayes/vignettes/tutorial-mhmm.html)
- [Estimation vignette mHMMbayes package](https://cran.r-project.org/web/packages/mHMMbayes/vignettes/estimation-mhmm.pdf)
- Example of multilevel HMM applied to [bipolar disorder](https://osf.io/preprints/psyarxiv/egp82/) using continuous input data
- Example of multilevel HMM applied to [nonverbal communication in patient-therapist dyads](https://doi.org/10.1016/j.jadr.2023.100635) using continuous input data
- Example of multilevel HMM for [neural spiking based behavioural event states](https://doi.org/10.1111/ejn.16065) using count data input


## Contact

For questions about this course, you can contact the instructor Emmeke ([e.aarts@uu.nl](mailto:e.aarts@uu.nl)) directly. 




    

