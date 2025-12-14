	PROGRAM PROPERTIES_SEAWATER
      IMPLICIT NONE

	!Deklarasi Ukuran Data Dinamis 
      INTEGER N_DATA_SET
      INTEGER I
      INTEGER STAT_ALLOC
      
	!Input Arrays (ALLOCATABLE)
      REAL, ALLOCATABLE :: S_BATCH(:), T_BATCH(:), P_BATCH(:)
      REAL, ALLOCATABLE :: R_IN_BATCH(:)
      REAL, ALLOCATABLE :: LAT_BATCH(:), PR_BATCH(:)
      
	!Deklarasi Variabel Hasil (ALLOCATABLE)
      REAL, ALLOCATABLE :: S_FINAL(:)
      REAL, ALLOCATABLE :: R_OUT_BATCH(:)
      REAL, ALLOCATABLE :: SVAN_RES(:), SIGMA_RES(:)
      REAL, ALLOCATABLE :: DEPTH_RES(:), TF_RES(:)
      REAL, ALLOCATABLE :: CPSW_RES(:), ATG_RES(:)
      REAL, ALLOCATABLE :: THETA_RES(:), SVEL_RES(:)
      
	!Deklarasi Eksternal Fungsi
      REAL SAL78_MIXED, TF, ATG, SVEL
      REAL SVAN, DEPTH, CPSW, THETA
      REAL RT35, RP_FUNCTION
      EXTERNAL SAL78_MIXED, TF, ATG, SVEL
      EXTERNAL SVAN, DEPTH, CPSW, THETA
      EXTERNAL RT35, RP_FUNCTION

	! Input Jumlah Data Dari Pengguna
      WRITE(*, '(A\)') ' Masukkan jumlah set data (N):'
      READ(*, *) N_DATA_SET
      
      IF (N_DATA_SET .LE. 0) THEN
         WRITE(*,*) 'Jumlah data harus lebih dari 0.Program dihentikan.'
         GO TO 999
      END IF
      
	! ALOKASI SEMUA ARRAY ---
      ALLOCATE(S_BATCH(N_DATA_SET), T_BATCH(N_DATA_SET), 
     1 P_BATCH(N_DATA_SET), R_IN_BATCH(N_DATA_SET), 
     2 LAT_BATCH(N_DATA_SET), PR_BATCH(N_DATA_SET), 
     3 S_FINAL(N_DATA_SET), R_OUT_BATCH(N_DATA_SET), 
     4 SVAN_RES(N_DATA_SET), SIGMA_RES(N_DATA_SET), 
     5 DEPTH_RES(N_DATA_SET), TF_RES(N_DATA_SET), 
     6 CPSW_RES(N_DATA_SET), ATG_RES(N_DATA_SET), 
     7 THETA_RES(N_DATA_SET), SVEL_RES(N_DATA_SET), 
     8 STAT=STAT_ALLOC)

      IF (STAT_ALLOC .NE. 0) THEN
         WRITE(*,*) 'ERROR: Gagal mengalokasikan array.'
         GO TO 999
      END IF

	!Input Set Data (R,T,P)
      WRITE(*,*) '------------------------------------'
      WRITE(*, '(A, I2, A)') ' Memasukkan', N_DATA_SET, 
     1 ' Set Data (R, T, P)'
      WRITE(*,*) ' Masukkan setiap nilai!'
      WRITE(*,*) '------------------------------------'

      DO I = 1, N_DATA_SET
         WRITE(*, '(A, I2, A)') ' Masukkan Set Data Ke-', I, ':'
         
	!Input R (Rasio Konduktivitas)
         WRITE(*, '(A\)') ' Rasio Konduktivitas R: '
         READ(*, *) R_IN_BATCH(I)
         
	!InputT (Suhu)
         WRITE(*, '(A\)') ' Suhu T (Celsius): '
         READ(*, *) T_BATCH(I)
         
	! Input  P (Tekanan) 
         WRITE(*, '(A\)') ' Tekanan P (decibars): '
         READ(*, *) P_BATCH(I)
         
         WRITE(*,*)
      END DO
      
	!Output Data Set
      WRITE(*,*) '-----------------------------------------'
      WRITE(*,*) ' Data Set yang Akan Diproses:'
      WRITE(*,*) '-----------------------------------------'
      
      DO I = 1, N_DATA_SET
          WRITE(*, '(A, I2, A, F7.4, A, F4.1, A, F8.1, A)') 
     1    ' Data Ke-', I, ' (R=', R_IN_BATCH(I), ', T=',
     2    T_BATCH(I), ', P=', P_BATCH(I), ')'
      END DO
      
      WRITE(*,*) '-----------------------------------------'
	WRITE(*,*)

      !1. Conductivity to Salinity Conversion (R, T, P) 
      WRITE(*,*) ' 1. Conductivity to Salinity Conversion'
      
      DO I = 1, N_DATA_SET
          S_FINAL(I) = SAL78_MIXED(R_IN_BATCH(I), T_BATCH(I), 
     1    P_BATCH(I), 0) ! M=0 (R -> S)
          S_BATCH(I) = S_FINAL(I)!Hasil Salinitas Disimpan Untuk Step Berikutnya
      END DO
      
	!Output Tabel Konversi 1
      WRITE(*, '(A)') '-----------------------------------------'//
	1				'----------------'
      WRITE(*, '(A)') ' No.| Input R | Suhu (C) | Tekanan (dbar) |'//
     1				' Salinitas (S)'
      WRITE(*, '(A)') '-------------------------------------------'//
	1				'--------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F10.1, F12.1, F17.4)') I, ' |', 
     1    R_IN_BATCH(I), T_BATCH(I), P_BATCH(I), S_FINAL(I)
      END DO
      WRITE(*, '(A)') '-----------------------------------------'//
	1				'----------------'
      WRITE(*,*)
      
	! 2. Salinity to Conductivity Conversion (S, T, P)
      WRITE(*,*) ' 2. Salinity to Conductivity Conversion '
      
      DO I = 1, N_DATA_SET
          R_OUT_BATCH(I) = SAL78_MIXED(S_FINAL(I), T_BATCH(I), 
     1    P_BATCH(I), 1) ! M=1 (S -> R)
      END DO
      
	!Output Tabel Konversi 2
      WRITE(*, '(A)') '-----------------------------------------'//
	1				'----------------------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu (C) | Tekanan (dbar) |'//
     1				' Ratio Konduktivitas (R)'
      WRITE(*, '(A)') '-----------------------------------------'//
	1				'----------------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F9.1, F13.1, F23.7)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), R_OUT_BATCH(I)
      END DO
      WRITE(*, '(A)') '----------------------------------------'//
	1				'-----------------------------'
      WRITE(*,*)

	!3. Specific Volume Anomaly and Density Anomaly of Seawater (S, T, P) 
      WRITE(*,*) ' 3. Specific Volume Anomaly and Density Anomaly '
      
      DO I = 1, N_DATA_SET
          SIGMA_RES(I) = 0.0
          SVAN_RES(I) = SVAN(S_FINAL(I), T_BATCH(I), P_BATCH(I), 
     1    SIGMA_RES(I))
      END DO
      
	!Output Tabel Konversi 3
      WRITE(*, '(A)') '-------------------------------------------'//
	1				'------------------------------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu (C) | Tekanan (dbar) |'//
     1				' SVAN [E-8 M^3/KG] | SIGMA [KG/M^3]'
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'------------------------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F9.1, F13.1, F20.4, F17.4)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), SVAN_RES(I), SIGMA_RES(I)
      END DO
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'------------------------------------'
      WRITE(*,*)

	! 4. Pressure to Dept Conversion (P, LAT) 
      WRITE(*,*) ' 4. Pressure to Dept Conversion '
      WRITE(*,*) 'Variabel Baru yang Diperlukan : Lintang LAT (derajat)'
      
      DO I = 1, N_DATA_SET
          WRITE(*, '(A, I2, A\)') ' Lintang LAT untuk Data Set ke-',
     1	I, ':'
          READ(*, *) LAT_BATCH(I)
          DEPTH_RES(I) = DEPTH(P_BATCH(I), LAT_BATCH(I))
      END DO
      
	!Output Tabel Konversi 4 
      WRITE(*, '(A)') '--------------------------------------'//
	1				'-------------------'
      WRITE(*, '(A)') ' No.| Tekanan (dbar) | Lintang (deg) |'//
     1				' Kedalaman (meter)'
      WRITE(*, '(A)') '--------------------------------------'//
	1				'-------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F11.1, F15.2, F18.3)') I, ' |', 
     1    P_BATCH(I), LAT_BATCH(I), DEPTH_RES(I)
      END DO
      WRITE(*, '(A)') '--------------------------------------'//
	1				'-------------------'
      WRITE(*,*)

	!5. Freezing Point Temperature (S, P) 
      WRITE(*,*) ' 5. Freezing Point Temperature'
      
      DO I = 1, N_DATA_SET
          TF_RES(I) = TF(S_FINAL(I), P_BATCH(I))
      END DO
      
	!Output Tabel Konversi 5 ---
      WRITE(*, '(A)') '------------------------------------------------'
      WRITE(*, '(A)') ' No.| Input S | Tekanan (dbar) | Titik Beku (C)'
      WRITE(*, '(A)') '------------------------------------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F15.1, F16.4)') I, ' |', 
     1    S_FINAL(I), P_BATCH(I), TF_RES(I)
      END DO
      WRITE(*, '(A)') '------------------------------------------------'
      WRITE(*,*)

	!6.Spesific Heat of Seawater (S, T, P) 
      WRITE(*,*) ' 6. Spesific Heat of Seawater'
      
      DO I = 1, N_DATA_SET
          CPSW_RES(I) = CPSW(S_FINAL(I), T_BATCH(I), P_BATCH(I))
      END DO
      
	!Output Tabel Konversi 6 
      WRITE(*, '(A)') '------------------------------------------'//
	1				'-----------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu (C) | Tekanan (dbar) |'//
     1				' CPSW [J/(KG*C)]'
      WRITE(*, '(A)') '------------------------------------------'//
	1				'-----------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F9.1, F13.1, F18.3)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), CPSW_RES(I)
      END DO
      WRITE(*, '(A)') '------------------------------------------'//
	1				'-----------------'
      WRITE(*,*)

	! 7. Adiabatic Lapse Rate (S, T, P) 
      WRITE(*,*) ' 7. Adiabtic Lapse Rate(ATG)'
      
      DO I = 1, N_DATA_SET
          ATG_RES(I) = ATG(S_FINAL(I), T_BATCH(I), P_BATCH(I))
      END DO
      
	!Output Tabel Konversi 7 
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'----------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu (C) | Tekanan (dbar) |'//
     1				' ATG [C/DECIBAR]'
      WRITE(*, '(A)') '------------------------------------------'//
	1				'------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F9.1, F13.1, E22.8)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), ATG_RES(I)
      END DO
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'----------------'
      WRITE(*,*)

	! 8. Potential Temperature (S, T, P, PR) 
      WRITE(*,*) ' 8. Potential Temperature (THETA)'
      WRITE(*,*) 'Variabel Baru yg Diperlukan:Tekanan Referensi PR '
      
      DO I = 1, N_DATA_SET
          WRITE(*, '(A, I2, A\)') ' Tekanan Referensi Data Set ke-',
     1	I, ': '
          READ(*, *) PR_BATCH(I)
          THETA_RES(I) = THETA(S_FINAL(I), T_BATCH(I), P_BATCH(I), 
     1    PR_BATCH(I))
      END DO
      
	!Output Tabel Konversi 8 
      WRITE(*, '(A)') '-------------------------------------------'//
	1				'----------------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu T | Tekanan P |'//
     1				'Ref. Press. PR | THETA (C)'
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'---------------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F8.1, F10.1, F14.1, F14.4)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), PR_BATCH(I), THETA_RES(I)
      END DO
      WRITE(*, '(A)') '--------------------------------------------'//
	1				'---------------------'
      WRITE(*,*)

	! 9. Sound Speed in Seawater(S, T, P) 
      WRITE(*,*) ' 9.  Sound Speed in Seawater(S, T, P)'
      
      DO I = 1, N_DATA_SET
          SVEL_RES(I) = SVEL(S_FINAL(I), T_BATCH(I), P_BATCH(I))
      END DO
      									   
	! Output Tabel Konversi 9
      WRITE(*, '(A)') '-----------------------------------------'//
	1				'--------------'
      WRITE(*, '(A)') ' No.| Input S | Suhu (C) | Tekanan (dbar) | '//
     1				'SVEL [M/S]'
      WRITE(*, '(A)') '------------------------------------------'//
	1				'-------------'
      DO I = 1, N_DATA_SET
          WRITE(*, '(I3, A, F9.4, F9.1, F13.1, F17.3)') I, ' |', 
     1    S_FINAL(I), T_BATCH(I), P_BATCH(I), SVEL_RES(I)
      END DO
      WRITE(*, '(A)') '---------------------------------------------'//
	1				'----------'
      WRITE(*,*)

      WRITE(*,*) 'SEMUA 9 KONVERSI TELAH SELESAI.'
      
	! Deallokasi Array 
      DEALLOCATE(S_BATCH, T_BATCH, P_BATCH, R_IN_BATCH, LAT_BATCH, 
     1           PR_BATCH, S_FINAL, R_OUT_BATCH, SVAN_RES, 
     2           SIGMA_RES, DEPTH_RES, TF_RES, CPSW_RES, 
     3           ATG_RES, THETA_RES, SVEL_RES)
      
999   END PROGRAM


	! Fungsi dan Subprogram Konversi

	!FUNGSI PENDUKUNG SALINITAS (Digunakan oleh SAL78_MIXED) 
      REAL FUNCTION RT35(T)
	! RT35 = C(35,T,O)/C(35,15,0)
      IMPLICIT NONE
      REAL T
      REAL C0, C1, C2, C3, C4
      DATA C0 /0.6766097/, C1 /2.00564E-2/, C2 /1.104259E-4/
      DATA C3 /-6.9698E-7/, C4 /1.0031E-9/
      RT35 = C0 + C1*T + C2*T**2 + C3*T**3 + C4*T**4
      END FUNCTION RT35

      REAL FUNCTION RP_FUNCTION(T, P, R)
	! RP = C(S,T,P)/C(S,T,0)
      IMPLICIT NONE
      REAL T, P, R
      REAL D1, D2, D3, D4, E1, E2, E3
      REAL NUM, DEN

      DATA E1 /2.070E-5/, E2 /-6.370E-10/, E3 /3.989E-15/
      DATA D1 /3.426E-2/, D2 /4.464E-4/, D3 /4.215E-1/
      DATA D4 /-3.107E-3/

      NUM = P*(E1 + P*(E2 + P*E3))
      DEN = 1.0 + D1*T + D2*T**2 + (D3 + D4*T)*R
      RP_FUNCTION = 1.0 + NUM / DEN
      END FUNCTION RP_FUNCTION

	!FUNGSI Algoritma 1 & 2: Konversi Salinitas (SAL78_MIXED)
      REAL FUNCTION SAL78_MIXED(CND, T, P, M)
	! M=0: CND->S; M=1: S->CND
      IMPLICIT NONE
      REAL CND, T, P
      INTEGER M

      REAL A(0:5), B(0:5), KAPPA
      REAL DT, R_IN, S_IN, S_N, DSDRT, RT, SQRT_RT, RT35_VAL
      REAL S_ATM, DELTA_S
      INTEGER N_ITER
      REAL RT35, RP_FUNCTION
      EXTERNAL RT35, RP_FUNCTION

	! Konstanta Polinomial Salinitas
      DATA A /0.0080, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081/
      DATA B /0.0005, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144/
      DATA KAPPA /0.0162/

      DT = T - 15.0
      RT35_VAL = RT35(T)

      IF (M .EQ. 1) GO TO 100

	! M=0: Konversi Konduktivitas ke Salinitas 
      R_IN = CND

      IF (R_IN .LE. 5E-4) GO TO 900
	!RT = R / (Rp * rt)
      RT = R_IN / (RP_FUNCTION(T, P, R_IN) * RT35_VAL)
      SQRT_RT = SQRT(ABS(RT))

      S_ATM = A(0) + SQRT_RT*(A(1) + SQRT_RT*(A(2) + SQRT_RT*(A(3) +
     1SQRT_RT*(A(4) + SQRT_RT*A(5)))))
      DELTA_S = DT / (1.0 + KAPPA*DT) * (B(0) + SQRT_RT*(B(1) +
     1SQRT_RT*(B(2) + SQRT_RT*(B(3) + SQRT_RT*(B(4) + SQRT_RT*B(5))))) )

      SAL78_MIXED = S_ATM + DELTA_S
      GO TO 999

	! M=1: Inversi Salinitas ke Conductivity Ratio (Iterasi Newton-Raphson)
100   S_IN = CND

      IF (S_IN .LE. 0.02) GO TO 900
	! First approximation: sqrt(S/35)
      SQRT_RT = SQRT(S_IN / 35.0)
      N_ITER = 0

200   CONTINUE 
      N_ITER = N_ITER + 1

	! Hitung S_n (Current Salinity)
      S_ATM = A(0) + SQRT_RT*(A(1) + SQRT_RT*(A(2) + SQRT_RT*(A(3) +
     1SQRT_RT*(A(4) + SQRT_RT*A(5)))))
      DELTA_S = DT / (1.0 + KAPPA*DT) * (B(0) + SQRT_RT*(B(1) +
     1SQRT_RT*(B(2) + SQRT_RT*(B(3) + SQRT_RT*(B(4) + SQRT_RT*B(5))))) )
      S_N = S_ATM + DELTA_S

	! Hitung Turunan dS/d(sqrt(Rt))
      DSDRT = 2.0*SQRT_RT*(A(2) + SQRT_RT*(1.5*A(3) +
     1SQRT_RT*(2.0*A(4) + SQRT_RT*2.5*A(5)))) + A(1) +
     2DT / (1.0 + KAPPA*DT) * (2.0*SQRT_RT*(B(2) +
     3SQRT_RT*(1.5*B(3) + SQRT_RT*(2.0*B(4) +
     4SQRT_RT*2.5*B(5)))) + B(1))

	! Newton-Raphson Step
      SQRT_RT = SQRT_RT + (S_IN - S_N) / DSDRT

      IF((ABS(S_IN - S_N) .GT. 1.0E-4) .AND. (N_ITER .LT. 10)) GO TO 200

	! Hitung R akhir (Solusi Persamaan Kuadratik)
      RT = SQRT_RT**2
      SAL78_MIXED = RT * RT35_VAL * RP_FUNCTION(T, P, RT)
      GO TO 999

900   SAL78_MIXED = 0.0
999   END FUNCTION SAL78_MIXED
	!FUNGSI Algoritma 3: Specific Volume Anomaly (SVAN) 
      REAL FUNCTION SVAN(S, T, PO, SIGMA)
	! Menghitung SVAN (10^-8 M^3/KG) dan SIGMA (KG/M^3)
      IMPLICIT NONE
      REAL S, T, PO
      REAL SIGMA
      REAL P_CONV, R3500, DR350
      PARAMETER (P_CONV = 0.1)
	! Konstanta dari EOS80
      DATA R3500, R4 /1028.1063, 4.8314E-4/, DR350 /28.106331/
	EQUIVALENCE (E,D_TERM,B1_TERM), (BW,B_TERM,R3),(C_TERM,A1_TERM,R2)
	EQUIVALENCE (AM,A_TERM,R1),(KM,KO,K)

      REAL P, SR, SIG, SVA, V350P, KO, K35, K, DK
      REAL GAM, PK, DR35P, DVAN, R4
	! Deklarasi variabel intermediate (Dibuat eksplisit)
      REAL R1, R2, R3
      REAL E, BW, AM, D_TERM, C_TERM, B1_TERM, A1_TERM, KM
      REAL B_TERM, A_TERM

      P = PO * P_CONV; SR = SQRT(ABS(S))

	! R1: Polinomial Rho_w - 1000 (Polinomial Densitas Air Murni)
      R1 = ((((6.536332E-9*T - 1.120083E-6)*T + 1.001685E-4)*T
     1- 9.095290E-3)*T + 6.793952E-2)*T - 28.263737
     
	! R2 & R3: Polinomial Salinitas (Rho(S,T,0))
      R2 = (((5.3875E-9*T - 8.2467E-7)*T + 7.6438E-5)*T - 4.0899E-3)
     1*T + 8.24493E-1
      R3 = (-1.6546E-6*T + 1.0227E-4)*T - 5.72466E-3
      
	! SIGMA at P=0 
      SIG = (R4*S + R3*SR + R2)*S + R1
	! SVAN at P=0 
      V350P = 1.0 / R3500
      SVA = SIG * V350P / (R3500 + SIG)
      SIGMA = SIG + DR350
      SVAN = SVA * 1.0E+8

      IF (P .EQ. 0.0) RETURN

	! HIGH PRESSURE TERM (DK dan K35) 
      E=(9.1697E-10*T+2.0816E-8)*T-9.9348E-7
      BW=(5.2787E-8*T-6.12293E-6)*T+3.47718E-5 
      B_TERM=BW+E*S 
      D_TERM=1.91075E-4
      C_TERM=(-1.6078E-6*T-1.0981E-5)*T+2.2838E-3
      AM=((-5.77905E-7*T+1.16092E-4)*T+1.43713E-3)*T-0.1194975
      A_TERM=(D_TERM*SR+C_TERM)*S+AM  
      B1_TERM=(-5.3009E-4*T+1.6483E-2)*T+7.944E-2
      A1_TERM=((-6.1670E-5*T+1.09987E-2)*T-0.603459)*T+54.6746
      KM=(((-5.155288E-5*T+1.360477E-2)*T-2.327105)*T
     1+148.4206)*T-1930.06 
	! KO: Term Salinitas untuk Bulk Modulus
      KO=(B1_TERM*SR+A1_TERM)*S+KM

	! DK = K(S,T,P) - K(35,0,P) 
      DK=(B_TERM*P+A_TERM)*P+KO
	! K35 = K(35,0,P) 
      K35 = (5.03217E-5*P + 3.359406)*P + 21582.27

	! Rumus Anomali
      GAM = P / K35
      PK = 1.0 - GAM
      
      SVA = SVA * PK + (V350P + SVA) * P * DK / (K35 * (K35 + DK))
      SVAN = SVA * 1.0E+8

	! Update SIGMA 
      V350P = V350P * PK
      DR35P = GAM / V350P
      DVAN = SVA / (V350P * (V350P + SVA))
      SIGMA = DR350 + DR35P - DVAN

      END FUNCTION SVAN
	

	!  FUNGSI Algoritma 4: Pressure to Depth Conversion (DEPTH) 
      REAL FUNCTION DEPTH(P, LAT)
	! Menghitung Kedalaman (meters) dari Tekanan (decibars) dan Lintang (degrees)
      IMPLICIT NONE
      REAL P, LAT
      REAL GR, X, P_P

      P_P = P
	! Hitung G(phi) (Gravity)
      X = SIN(LAT/57.29578)
      X = X*X
	! GR = Gravity (g(phi) + correction)
      GR = 9.780318*(1.0 + (5.2788E-3 + 2.36E-5*X)*X) + 1.092E-6*P_P
	! Integral v(35,0,p)dp (Rumus 24)
      DEPTH = (((-1.82E-15*P_P + 2.279E-10)*P_P - 2.2512E-5)*P_P
     1+ 9.72659)*P_P
	! DEPTH = Integral / GR (Rumus 25)
      DEPTH = DEPTH / GR
      END FUNCTION DEPTH

	!FUNGSI Algoritma 5: Freezing Point Temperature (TF)
      REAL FUNCTION TF(S, P)
	! Menghitung Titik Beku (Celsius)
      IMPLICIT NONE
      REAL S, P
      REAL A0, A1, A2, B_CONST
	! Konstanta Rumus 
      DATA A0 /-0.0575/, A1 /1.710523E-3/, A2 /-2.154996E-4/
      DATA B_CONST /-7.53E-4/

      TF = (A0 + A1*SQRT(ABS(S)) + A2*S)*S + B_CONST*P
      END FUNCTION TF

	! FUNGSI Algoritma 6: Specific Heat of Seawater (CPSW)
      REAL FUNCTION CPSW(S, T, PO)
	! Menghitung Kalor Spesifik (J/(KG*C))
      IMPLICIT NONE
      REAL S, T, PO
      REAL P, SR, A, B, C, CPO, CP1, CP2

      P = PO / 10.0
      SR = SQRT(ABS(S))
	!CPO: Cp(S,t,0) 
      A = (-1.38385E-3*T + 0.1072763)*T - 7.643575
      B = (5.148E-5*T - 4.07718E-3)*T + 0.1770383
      C = (((2.093236E-5*T - 2.654387E-3)*T + 0.1412855)*T
     1- 3.720283)*T + 4217.4
      CPO = (B*SR + A)*S + C
	! CP1: Delta_1 Cp(0,t,p) (Rumus 795-796)
      A = (((1.7168E-8*T + 2.0357E-6)*T - 3.13885E-4)*T + 1.45747E-2)
     1*T - 0.49592
      B = (((2.2956E-11*T - 4.0027E-9)*T + 2.87533E-7)*T
     1- 1.08645E-5)*T + 2.4931E-4
      C = ((6.136E-13*T - 6.5637E-11)*T + 2.6380E-9)*T - 5.422E-8
      CP1 = ((C*P + B)*P + A)*P
	! CP2: Delta_2 Cp(S,t,p) (Rumus 820)
      A = (((-2.9179E-10*T + 2.5941E-8)*T + 9.802E-7)*T - 1.28315E-4)
     1*T + 4.9247E-3
      B = (3.122E-8*T - 1.517E-6)*T - 1.2331E-4
      A = (A + B*SR)*S
      B = ((1.8448E-11*T - 2.3905E-9)*T + 1.17054E-7)*T - 2.9558E-6
      B = (B + 9.971E-8*SR)*S
      C = (3.513E-13*T - 1.7682E-11)*T + 5.540E-10
      C = (C - 1.4300E-12*T*SR)*S
      CP2 = ((C*P + B)*P + A)*P
	! CPSW = CPO + CP1 + CP2
      CPSW = CPO + CP1 + CP2
      END FUNCTION CPSW

	!FUNGSI Algoritma 7: Adiabatic Lapse Rate (ATG)
      REAL FUNCTION ATG(S, T, P)
	! Menghitung Tingkat Kelajuan Adiabatik (C/dbar)
      IMPLICIT NONE
      REAL S, T, P
      REAL DS

      DS = S - 35.0
      ATG = ((((-2.1687E-16*T + 1.8676E-14)*T - 4.6206E-13) * P +
     1((2.7759E-12*T - 1.1351E-10)*DS + ((-5.4481E-14*T +
     28.733E-12)*T - 6.7795E-10)*T + 1.8741E-8)) * P +
     3(-4.2393E-8*T + 1.8932E-6)*DS +
     4((6.6228E-10*T - 6.836E-8)*T + 8.5258E-6)*T + 3.5803E-5)
      END FUNCTION ATG

	! FUNGSI Algoritma 8: Potential Temperature (THETA) 
      REAL FUNCTION THETA(S, TO, PO, PR)
	! Menghitung Theta menggunakan integrasi Runge-Kutta Orde-4
      IMPLICIT NONE
      REAL S, TO, PO, PR
      REAL H, P, T, XK, Q
      REAL ATG
      EXTERNAL ATG

      P = PO
      T = TO
      H = PR - P
	! Koefisien Runge-Kutta Orde-4 
	! Langkah 1: K1
      XK = H * ATG(S, T, P)
      T = T + 0.5 * XK
      Q = XK
      P = P + 0.5 * H
	! Langkah 2: K2
      XK = H * ATG(S, T, P)
      T = T + 0.29289322 * (XK - Q)
      Q = 0.58578644 * XK + 0.121320344 * Q
	! Langkah 3: K3
      XK = H * ATG(S, T, P)
      T = T + 1.707106781 * (XK - Q)
      Q = 3.414213562 * XK - 4.121321344 * Q 
      P = P + 0.5 * H
	! Langkah 4: K4 (Final Step)
      XK = H * ATG(S, T, P)
	! THETA = T_prev + (K4 - 2*q3) / 6.0 (Rumus 1018)
      THETA = T + (XK - 2.0*Q) / 6.0
      END FUNCTION THETA

	! FUNGSI Algoritma 9: Sound Speed in Seawater (SVEL) 
      REAL FUNCTION SVEL(S, T, PO)
	! Menghitung Kecepatan Suara (M/S)
      IMPLICIT NONE
      REAL S, T, PO
      REAL P, SR, A, B, C, D
      REAL A0, A1, A2, A3, B0, B1, C0, C1, C2, C3

      P = PO / 10.0
      SR = SQRT(ABS(S))
	! D: S**2 TERM 
      D = 1.727E-3 - 7.9836E-6*P
	! B: S**3/2 TERM 
      B1 = 7.3637E-5 + 1.7945E-7*T
      B0 = -1.922E-2 - 4.42E-5*T
      B = B0 + B1*P
	! A: S**1 TERM 
      A3 = (-3.389E-13*T + 6.649E-12)*T + 1.100E-10
      A2 = ((7.988E-12*T - 1.6002E-10)*T + 9.1041E-9)*T - 3.9064E-7
      A1 = (((-2.0122E-10*T + 1.0507E-8)*T - 6.4885E-8)*T -
     11.2580E-5)*T + 9.4742E-5
      A0 = (((-3.21E-8*T + 2.006E-6)*T + 7.164E-5)*T - 1.262E-2)*T
     1+ 1.389
      A = ((A3*P + A2)*P + A1)*P + A0

	! C: S**0 TERM (Cw) (Rumus 1087-1090)
      C3 = (-2.3643E-12*T + 3.8504E-10)*T - 9.7729E-9
      C2 = (((1.0405E-12*T - 2.5335E-10)*T + 2.5974E-8)*T -
     11.7107E-6)*T + 3.1260E-5
      C1 = (((-6.1185E-10*T + 1.3621E-7)*T - 8.1788E-6)*T +
     16.8982E-4)*T + 0.153563
      C0 = ((((3.1464E-9*T - 1.47800E-6)*T + 3.3420E-4)*T -
     15.80852E-2)*T + 5.03711)*T + 1402.388
      C = ((C3*P + C2)*P + C1)*P + C0
	! SVEL = Cw + A*S + B*S^(3/2) + D*S^2
      SVEL = C + (A + B*SR + D*S)*S
      END FUNCTION SVEL