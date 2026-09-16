$PROB SAMPLE (2-COMPARTMENT MODEL)

$PLUGIN autodec nm-vars

$THETA
0.1 // TVCL [L/day] : Clearance
1 // TVV1 [L] : Volume of distribution of the central compartment
0.1 // TVQ [L/day] : Inter-compartmental clearance
1 // TVV2 [L] : Volume of distribution of the peripheral compartment
0.1 // W [-] : Proportional error
0.5 // V1SEX1 [-] : Sex effect on V1
0.5 // V1SEX2 [-] : Sex effect on V1
0.5 // V1WT [-] : Weight effect on V1

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
V1COV = V1WT * V1SEX;
TVV1 = THETA(2);
V1 = TVV1 * V1COV * EXP(ETA(2));
TVQ = THETA(3);
Q = TVQ * EXP(ETA(3));
TVV2 = THETA(4);
V2 = TVV2 * EXP(ETA(4));
KE = CL/V1;
K12 = Q/V1;
K21 = Q/V2;

$OMEGA @block
0.1 // CL
0.1 0.1 // V1
$OMEGA
0.1 // Q
0.1 // V2

$SIGMA
1 // Proportional residual error

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
