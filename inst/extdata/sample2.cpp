$PROB SAMPLE (2-COMPARTMENT MODEL)

$PLUGIN autodec nm-vars

$THETA @annotated
0.1 : CL [L/day]
1 : V1 [L]
0.1 : Q [L/day]
1 : V2 [L]
0.1 : W [-]
0.5 : V1SEX1 [-]
0.5 : V1SEX2 [-]
0.5 : V1WT [-]

$PARAM @covariates
WT = 1
SEX = 1
AGE = 1

$CMT
CENTRAL
PERIPHERAL

$PK
TVCL = THETA(1);
CL = TVCL * EXP(ETA(1));
V1WT = (WT - 70) * THETA(6);
if (SEX==1) {
V1SEX = THETA(7);
} else {
V1SEX = THETA(8);
}
V2COV = V1WT * V1SEX;
TVV1 = THETA(2);
V1 = TVV1 * EXP(ETA(2));
TVQ = THETA(3);
Q = TVQ * EXP(ETA(3));
TVV2 = THETA(4);
V2 = TVV2 * EXP(ETA(4));
KE = CL/V1;
K12 = Q/V1;
K21 = Q/V2;

$OMEGA @block
0.1
0.1 0.1
$OMEGA
0.1
0.1

$SIGMA
1

$DES
if (T<=4) {
KE = 0;
}
DADT(1) = -K12 * A(1) + K21 * A(2) - KE * A(1);
DADT(2) = K12 * A(1) - K21 * A(2);

$ERROR
IPRED = A(1)/V1;
if (AGE>=40) {
IPRED = IPRED * 2;
}
W = THETA(5);
Y = IPRED * (1 + W * EPS(1));
// IWRES = (DV - IPRED)/(IPRED * W);

$CAPTURE
EVID CMT AMT WT SEX AGE IPRED W Y
