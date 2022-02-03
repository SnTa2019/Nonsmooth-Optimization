!*************************************************************************

!*     Bundle enrichment method (BEM-DC) is an algorithm for solving     *
!*     nonsmooth difference of convex (DC) programming problems          *
                                                                      
!*     by Napsu Karmitsa, Adil Bagirov and Sona Taheri                   *
!*     (last modified 02.02.2022).
!*     The code is partly based on the code snippets by Kaisa Joki       *
!*     and it uses a quadratic solver by Ladislav Luksan.                *

!*     The software is free for academic teaching and research           *
!*     purposes but we ask you to refer the appropriate references       *
!*     given below, if you use it.                                       *


!*************************************************************************
!*     Codes included:  
!*
!*
!*     bem.f95           - Main program for BEM-DC.
!*     initbem.f95       - Initialization of parameters for BEM-DC.
!*     parameters.f95    - Global parameters and constants.
!*     functions.f95     - Computation of DC components f_1 and f_2 and their subgradients.
!*     plqdf.f           - Quadratic solver by L. Luksan.
!*
!*
!*     Calling sequence:
!*
!*       ./bem
!*
!*     Give your functions and starting point in functions.f03.
!*     If you want to tune other parameters, modify initbem.f03.
!* 
!*     As default the results are written in
!*        
!*     results1.txt          - value of the objective, numbers of evaluations etc. 
!*     results2.txt          - optimal point.
!*
!*
!*
!*     Reference:
!*     M. Gaudioso, S. Taheri, A. Bagirov, N. Karmitsa, 
!*     "Bundle enrichment method for nonsmooth DC programming", submitted, 2022. 


!*    Acknowledgements: 
!*    The research work by Adil Bagirov was supported by the Australian Government through 
!*    Australian Research Council's Discovery Projects funding scheme (Project No. DP190100580).
