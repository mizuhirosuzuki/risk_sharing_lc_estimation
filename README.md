# Estimation of risk-sharing model with limited commitment

This repo creates [a website](https://mizuhirosuzuki.github.io/risk_sharing_lc_estimation/) which showcases how to conduct a structural estimation of a risk-sharing model with limited commitment.
It contains explanations on both theory and empirics.
The code is largely based on the replication package of [Laczó (2015)](https://onlinelibrary.wiley.com/doi/abs/10.1111/jeea.12115).
However, the purpose of this website is *not* to reproduce the paper's results.
Actually, I simplified the estimation by, for example, reducing the parameters to estimate to focus on the core idea.
Also I do not estimate the case with heterogeneous preferences, which was the main focus of the paper, because my focus here is to show the base risk-sharing model with limited commitment.
The R code is greatly refactored, mainly for my learning purpose, which is another reason why some of the simulation results do not match although the original R script properly set the random seed for reproducibility.

Here I would like to thank [Sarolta Laczó](https://saroltalaczo.wixsite.com/home) for providing the replication package to the journal website.
I believe this is a good example of how such replication package can help people's understanding and learning, not just for verifying the reproducibility of studies.

Of course, any errors are my own.
And any comments/feedback are greatly appreciated.
If you use GitHub, please use the [issue page](https://github.com/mizuhirosuzuki/risk_sharing_lc_estimation/issues) of the [GitHub repo](https://github.com/mizuhirosuzuki/risk_sharing_lc_estimation).
Otherwise, please send an email to mizuhiro.suzuki@gmail.com .
