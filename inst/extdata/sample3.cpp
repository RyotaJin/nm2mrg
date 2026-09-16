$PROB SAMPLE (M3-LIKE MODEL)

$PLUGIN autodec nm-vars

$THETA
0.1 // TVCL [L/day] : Clearance
1 // TVV1 [L] : Central Volume
0.1 // W [-] : Proportional error

$CMT
CENTRAL

$PK
TVCL = THETA(1);
CL = TVCL * EXP(ETA(1));
TVV1 = THETA(2);
V1 = TVV1 * EXP(ETA(2));
KE = CL/V1;

$OMEGA @block
0.1 // CL
0.1 0.1 // V1

$SIGMA
1 // Proportional residual error

$DES
DADT(1) = -KE * A(1);

$ERROR
IPRED = A(1)/V1;
W = THETA(3);
Y = IPRED * (1 + W * EPS(1));

$CAPTURE
EVID CMT AMT IPRED W Y
