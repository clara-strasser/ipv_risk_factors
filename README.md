# ipv_risk_factors
This project was created for my master thesis at the LMU Munich.

**Thesis Title**

Unveiling the Determinants of Psychological Intimate Partner Violence (IPV) Against Mothers in Mexico - Insights from Model-Based Boosting

**Abstract**

Psychological violence is the most prevalent form of intimate partner violence (IPV) against
women in Mexico. This study aims to uncover the risk and protective factors of psychological
IPV, with a particular focus on women with children. For this purpose, an ecological approach
is adopted by including factors at the individual, relationship, community, and societal levels,
which results in a multidimensional data set. Model-based boosting, a machine learning algorithm, is used to estimate the relationship between the constructed factors and the women’s risk
of experiencing psychological IPV. The findings reveal an elevated risk of psychological IPV for
women exposed to violence in childhood. This increased risk applies likewise to women whose
partners experienced or witnessed violence in childhood. Additionally, employed women face
a higher risk of suffering from IPV than unemployed women. In contrast, women with strong
social support networks are less likely to experience psychological IPV. This protective effect
holds also for women with autonomy in decision-making regarding their sexual, professional,
and economic lives, as well as for those living in households where housework is equitably
distributed.

**Framework**
My master thesis extends the paper by Torres Munguia and Martinez-Zarzoso (2022).

The code to replicate the analysis can be provided by the authors upon request.

**Requirements**

- The main data set is publicly available: [ENDIREH 2021](https://www.inegi.org.mx/programas/endireh/2021/)

- Other data sets are: 
                      * [Homicidios](https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/defuncioneshom.asp?s=est/)
                      
                      * [Intercensal](https://en.www.inegi.org.mx/app/descarga/)
                      
                      * [Migración](https://www.inegi.org.mx/temas/migracion/)
                      
                      * [CONEVAL](https://www.coneval.org.mx/Medicion/Paginas/Cohesion_Social.aspx)
                      
                      * [UNDP](https://www.idhmunicipalmexico.org/)
                      
                      * [CONAPO](https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372)
                      
                      * [CNGMD](https://www.inegi.org.mx/programas/cngmd/2015/)
                      
                      * [INEGI](https://www.inegi.org.mx/temas/mg/)
                      
                      * [ENVIPE](https://www.inegi.org.mx/programas/envipe/2022/)
                      
                      * [ENCIG](https://www.inegi.org.mx/programas/encig/2019/)
                      

**R-Packages**

The *renv* R-package is used to create reproducible environments for the R-project.

See more about renv: [renv](https://rstudio.github.io/renv/articles/renv.html)

**Code Structure**

The code structure is documented in the *doc.txt*.

**References**

Torres Munguía, J. A., & Martínez-Zarzoso, I. (2022). Determinants of Emotional Intimate Partner Violence against Women and Girls with Children in Mexican Households: An Ecological Framework. Journal of Interpersonal Violence, 37(23-24), NP22704-NP22731. DOI: 10.1177/08862605211072179


