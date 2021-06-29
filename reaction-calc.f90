module reactionCalc
  use,intrinsic :: iso_fortran_env
  use matrixCoeff
  implicit none

contains
subroutine reaction(TempPointer, ReactionTerm)
  real(real64), dimension(22,1), intent(in) :: TempPointer
  real(real64), dimension(22,1), intent(out) :: ReactionTerm
  
  real(real64) :: Kdeath_ALG = 0.1, Kdeath_CON = 0.05, Kgrowth_ALG = 2.0, Kgrowth_CON = 0.0002, Kgrowth_Haer = 2.0, &
            Kgrowth_Anox = 1.6, Kgrowth_N1 = 0.8, Kgrowth_N2 = 1.1, Khyd = 3.0, Kresp_ALG = 0.1, Kresp_CON = 0.05, &
            Kresp_Haer = 0.2, Kresp_Anox = 0.1, Kresp_N1 = 0.05, Kresp_N2 = 0.05, Keq1 = 0.001, Keq2 = 0.0001, &
            Keqw = 0.0001, KeqN = 0.0001, KeqP = 0.0001, KeqSO = 0.00000002, KHPO4_ALG = 0.02, KHPO4_Haer = 0.02, &
            KHPO4_Anox = 0.02, KHPO4_N1 = 0.02, KHPO4_N2 = 0.02, KN_ALG = 0.1, KNH4_ALG = 0.1, KNH_aer = 0.2, &
            KNH4_N1 = 0.5, K1 = 500.0, KNO3_Anox = 0.5, KNO2_Anox = 0.2, KNO2_N2 = 0.5, KO2_ALG = 0.2, KO2_CON = 0.5, &
            KO2_Haer = 0.2, KO2_N1 = 0.5, KO2_N2 = 0.5, KS_Haer = 2.0, KS_Anox = 2.0, Beta_ALG = 0.046, Beta_CON = 0.08, &
            Beta_H = 0.07, Beta_HYD= 0.07, Beta_N1 = 0.098, Beta_N2 = 0.069, Temp0 = 20.0
  real(real64) KKeqw, KKeq1, KKeq2, KKeqN, KKeqP, KKeqSO, SunlightSd
  real(real64) :: ALLRiver_Temp = 18.3
  real(real64) :: Sunlight=150.0, SavinovCon=0.0, Cloud=0.0
  real(real64), dimension(28,22) :: Chemsp
  integer Item_Pointer

  KKeqw = 10.0**((-4470.99) / (273.15 + ALLRiver_Temp) + 12.0875 - 0.01706 * (273.15 + ALLRiver_Temp))
  KKeq1 = 10.0**(17.843 - (3404.71 / (273.15 + ALLRiver_Temp)) - 0.032786 * (273.15 + ALLRiver_Temp))
  KKeq2 = 10.0**(9.494-2902.39 / (273.15 + ALLRiver_Temp) - 0.02379 * (273.15 + ALLRiver_Temp))
  KKeqN = 10.0**(2.891 - 2727.0 / (273.15 + ALLRiver_Temp))
  KKeqP = 10.0**((-3.46) - (219.4 / (273.15 + ALLRiver_Temp)))
  KKeqSO = 12.0 * 40.0 * 10.0 **(19.87 - (3059 / (273.15 + ALLRiver_Temp)) - (0.04035 * (273.15 + ALLRiver_Temp))) 

  SunlightSd = Sunlight * ( 1 - ( 1 - SavinovCon ) * Cloud )

  call matrix(Chemsp)
  
  do Item_Pointer = 1, 22;
    ReactionTerm(Item_Pointer, 1) = Chemsp(1,Item_Pointer) * Kgrowth_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(1,1), KS_Haer) * Frac(TempPointer(9,1), KO2_Haer) * & 
        Frac(TempPointer(3,1) + TempPointer(4,1), KNH_aer) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Haer) * TempPointer(16, 1) &  
        + Chemsp(2,Item_Pointer) * Kgrowth_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(1,1), KS_Haer) * Frac(TempPointer(9,1), KO2_Haer) * &
        Frac(KNH_aer, TempPointer(3,1) + TempPointer(4,1))  * &
        Frac(KNH_aer, TempPointer(6,1)) * Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Haer) * TempPointer(16,1) &  
        + Chemsp(3,Item_Pointer) * Kresp_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_Haer) * TempPointer(16,1) &  
        + Chemsp(4,Item_Pointer) * Kgrowth_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(1,1), KS_Anox) * Frac(KO2_Haer, TempPointer(9,1)) * Frac(TempPointer(6,1), KNO3_Anox) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Anox) * TempPointer(16,1) & 
        + Chemsp(5,Item_Pointer) * Kgrowth_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(1,1), KS_Anox) * Frac(KO2_Haer, TempPointer(9,1)) * Frac(TempPointer(5,1), KNO2_Anox) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Anox) * TempPointer(16,1) &    
        + Chemsp(6,Item_Pointer) * Kresp_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
        Frac(KO2_Haer, TempPointer(9,1)) * Frac(TempPointer(6,1), KNO3_Anox) * TempPointer(16,1) & 
        + Chemsp(7,Item_Pointer) * Kgrowth_N1 * exp(Beta_N1 * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_N1) * Frac(TempPointer(3,1) + TempPointer(4,1), KNH4_N1) *&
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_N1) * TempPointer(17,1) &  
        + Chemsp(8,Item_Pointer) * Kresp_N1 * exp(Beta_N1 * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_N1) * TempPointer(17,1) & 
        + Chemsp(9,Item_Pointer) * Kgrowth_N2 * exp(Beta_N2 * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_N2) * Frac(TempPointer(5,1), KNO2_N2) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_N2) * TempPointer(18,1) &  
        + Chemsp(10,Item_Pointer) * Kresp_N2 * exp(Beta_N2 * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_N2) * TempPointer(18,1) &  	                            
        + Chemsp(11,Item_Pointer) * Kgrowth_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(3,1) + TempPointer(4,1) + TempPointer(6,1), KN_ALG) * &
        Frac(TempPointer(3,1) + TempPointer(4,1), KNH4_ALG) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_ALG) * (SunlightSd / K1) * exp(1 - (Sunlightsd/K1)) * &
        TempPointer(19,1) & 
        + Chemsp(12,Item_Pointer) * Kgrowth_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(3,1) + TempPointer(4,1) + TempPointer(6,1), KN_ALG) * &
        Frac(KNH4_ALG, TempPointer(3,1) + TempPointer(4,1)) * &
        Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_ALG) * (Sunlightsd / K1) * exp(1 - (Sunlightsd / K1)) * &
        TempPointer(19,1) &  
        + Chemsp(13,Item_Pointer) * Kresp_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_ALG) * TempPointer(19,1) & 
        + Chemsp(14,Item_Pointer) * Kdeath_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * TempPointer(19,1) & 
        + Chemsp(15,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(19,1) * TempPointer(20,1) & 
        + Chemsp(16,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(21,1) * TempPointer(20,1) & 
        + Chemsp(17,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(16, 1) * TempPointer(20,1) &  
        + Chemsp(18,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(17,1) * TempPointer(20,1) & 
        + Chemsp(19,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(18,1) * TempPointer(20,1) &   
        + Chemsp(20,Item_Pointer) * Kresp_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
        Frac(TempPointer(9,1), KO2_CON) * TempPointer(20,1) & 		                            
        + Chemsp(21,Item_Pointer) * Kdeath_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * TempPointer(20,1) & 
        + Chemsp(22,Item_Pointer) * Khyd * exp(Beta_HYD * (ALLRiver_Temp - Temp0)) * TempPointer(21,1) &  
        + Chemsp(23,Item_Pointer) * Keq1 * (TempPointer(10,1) - TempPointer(13,1) * TempPointer(11,1) / KKeq1) & 
        + Chemsp(24,Item_Pointer) * Keq2 * (TempPointer(11,1) - TempPointer(13,1) * TempPointer(12,1) / KKeq2) & 
        + Chemsp(25,Item_Pointer) * Keqw * (1.0 - TempPointer(13,1) * TempPointer(14,1) / KKeqw) & 
        + Chemsp(26,Item_Pointer) * KeqN * (TempPointer(3,1) - TempPointer(13,1) * TempPointer(4,1) / KKeqN) & 
        + Chemsp(27,Item_Pointer) * KeqP * (TempPointer(8,1) - TempPointer(13,1) * TempPointer(7,1) / KKeqP) &   
        + Chemsp(28,Item_Pointer) * KeqSO * (1.0 - TempPointer(15, 1) * TempPointer(12,1) / KKeqSO)
  end do
end subroutine reaction

  real(real64) function Frac(x,y)
      real(real64) x,y
      
      Frac = (x) / (x + y)
      
      if (x + y == 0) then
          Frac = 0
      end if 

  end function Frac

end module reactionCalc