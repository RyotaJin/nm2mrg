$PROBLEM SAMPLE (ERROR WITHOUT MARKER)

$INPUT ID TIME CMT EVID DV AMT

$DATA data.csv

$SUBROUTINE ADVAN13

$MODEL
COMP=(CENTRAL)

$PK
TVCL = THETA(1)
CL = TVCL * EXP(ETA(1))

TVV1 = THETA(2)
V1 = TVV1 * EXP(ETA(2))

KE = CL / V1

$DES
DADT(1) = - KE * A(1)

$ERROR
IPRED = A(1) / V1
W = THETA(3)
Y = IPRED * (1 + W * EPS(1))
IWRES = (DV - IPRED) / (IPRED * W)

$THETA
(0, 0.1) ; TVCL [L/day] : Clearance
(0, 1)   ; TVV1 [L]     : Central Volume
(0, 0.1) ; W    [-]     : Proportional error

$OMEGA BLOCK(2)
0.1     ; CL
0.1 0.1 ; V1

$SIGMA
1 FIX ; Proportional residual error

$ESTIMATION

$COVARIANCE