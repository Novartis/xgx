#Solutions available at links below:
#http://chbslx0132.eu.novartis.net/svn/MS_MATERIAL/4_Methodology/Identifiability/Work/001_PK_1cmt_2cmt/doc/Approx_2cmt_with_1cmt_01.pdf
#http://chbslx0132.eu.novartis.net/svn/MS_MATERIAL/4_Methodology/Identifiability/References/Review/PFIM_PKPD_library.pdf
#http://www.pfim.biostat.fr/PFIM_PKPD_library.pdf

library(dplyr)

micro2macro_bolus_1cmt = function(q) {
  q = transmute(p,
                dose=dose,
                C   = dose/Vc,
                ke  = CL/Vc)
}
macro2micro_bolus_1cmt = function(q) {
  p = transmute(q,
                dose=dose,
                Vc = dose/C,
                CL = ke*Vc)
}

micro2macro_oral1_1cmt = function(q) {
  p = transmute(q,
                dose=dose,
                ka = ka,
                F  = F,
                C  = F*dose/Vc,
                ke = CL/Vc)
}
macro2micro_oral1_1cmt = function(q) {
  p = transmute(q,
                dose=dose,
                ka = ka,
                F  = F,
                Vc = ka*F*dose/(C*(ka-ke)),
                CL = ke*Vc)
}

micro2macro_bolus_2cmt = function(p) {
  p = mutate(p,
             k12 = Q/Vc,
             k21 = Q/Vp,
             ke  = CL/Vc)
  
  q = transmute(p,
                dose = dose,
                alpha= .5*(k12+k21+ke + sqrt( (k12+k21+ke)^2 - 4*ke*k21)),
                beta = .5*(k12+k21+ke - sqrt( (k12+k21+ke)^2 - 4*ke*k21)),
                A    = dose*(k21-alpha)/(Vc*(beta-alpha)),
                Ap   = dose* k12       /(Vc*(beta-alpha)), #peripheral 1
                B    = dose*(k21-beta) /(Vc*(alpha-beta)),
                Bp   = dose* k12       /(Vc*(alpha-beta))) #peripheral 2  
  return(q)
}

macro2micro_bolus_2cmt = function(q) {
  #2cmt with bolus dose
  #C = Abolus*exp(-alpha*t) + Bbolus*exp(-beta*t)
  
  p = q %>% 
    mutate( k21     = (A*beta+alpha*B)/(A+B),
            ke      = alpha*beta/k21,
            k12     = alpha+beta-k21-ke,
            
            Vc      = dose/(A+B),
            CL      = ke*Vc,
            Q       = Vc*k12,
            Vp      = Q/k21) %>%
    select(dose,CL,Vc,Vp,Q)
  return(p)
}

micro2macro_oral1_2cmt = function(p) {
  p = mutate(p,
             k12 = Q/Vc,
             k21 = Q/Vp,
             ke  = CL/Vc)
  
  q = transmute(p,
                dose = dose,
                F    = F,
                ka   = ka,
                alpha= .5*(k12+k21+ke + sqrt( (k12+k21+ke)^2 - 4*ke*k21)),
                beta = .5*(k12+k21+ke - sqrt( (k12+k21+ke)^2 - 4*ke*k21)),
                A    = ka*F*dose*(k21-alpha)/(Vc*(ka-alpha)*(beta-alpha)),
                Ap   = ka*F*dose* k12       /(Vc*(ka-alpha)*(beta-alpha)), #peripheral 1
                B    = ka*F*dose*(k21-beta) /(Vc*(ka-beta) *(alpha-beta)),
                Bp   = ka*F*dose* k12       /(Vc*(ka-beta) *(alpha-beta))) #peripheral 2  
  return(q)
}

macro2micro_oral1_2cmt = function(q) {
  #2cmt with oral absorption
  #C = A*exp(-alpha*t) + B*exp(-beta*t) - (A+B)*exp(-ka*t)
  #2cmt with bolus dose
  #C = Abolus*exp(-alpha*t) + Bbolus*exp(-beta*t)
  
  p = q %>% 
    mutate( Abolus  = A*(ka-alpha)/ka,
            Bbolus  = B*(ka-beta )/ka,
            
            k21     = (Abolus*beta+alpha*Bbolus)/(Abolus+Bbolus),
            ke      = alpha*beta/k21,
            k12     = alpha+beta-k21-ke,
            
            Vc      = F*dose/(Abolus+Bbolus),
            CL      = ke*Vc,
            Q       = Vc*k12,
            Vp      = Q/k21) %>%
    select(dose,F,ka,CL,Vc,Vp,Q)
  return(p)
}