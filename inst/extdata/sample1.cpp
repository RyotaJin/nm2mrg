$PROB SAMPLE (2-COMPARTMENT MODEL)

$PLUGIN autodec nm-vars

$THETA
0.1 // TVCL [L/day] : Clearance
1 // TVV1 [L] : Central Volume
0.1 // TVQ [L/day] : Intercompartmental Clearance
1 // TVV2 [L] : Peripheral Volume
0.1 // W [-] : Within-Subject Variability

$CMT
CENTRAL
PERIPHERAL

$PK
TVCL = THETA(1);
CL = TVCL * EXP(ETA(1));
TVV1 = THETA(2);
V1 = TVV1 * EXP(ETA(2));
TVQ = THETA(3);
Q = TVQ * EXP(ETA(3));
TVV2 = THETA(4);
V2 = TVV2 * EXP(ETA(4));
KE = CL/V1;
K12 = Q/V1;
K21 = Q/V2;
A_0(1) = 0;
A_0(2) = 0;

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
W = THETA(5);
Y = IPRED * (1 + W * EPS(1));
// IWRES = (DV - IPRED)/(IPRED * W);

$CAPTURE
EVID CMT AMT IPRED W Y
