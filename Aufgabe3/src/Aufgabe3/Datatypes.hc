/* GHC_PACKAGES base integer-gmp ghc-prim rts
*/
#include "Stg.h"
EI_(Aufgabe3ziDatatypes_zdwtourAnwenden_info);
StgWord Aufgabe3ziDatatypes_zdwtourAnwenden_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwtourAnwenden_info
};


EI_(Aufgabe3ziDatatypes_zdwtourAnwenden_info);
FN_(Aufgabe3ziDatatypes_zdwtourAnwenden_slow) {
FB_
R2.w = *Sp;
R3.w = Sp[1];
R4.w = Sp[2];
R5.w = Sp[3];
R6.w = Sp[4];
Sp=Sp+5;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtourAnwenden_info);
FE_
}

static StgWord s1dK_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1dK_ret) {
W_ _s1dL;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1ef;
_s1dL = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dL;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1ef:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dJ_info[] = {
0x1UL, 0x22UL
};

II_(s1dK_info);
IF_(s1dJ_ret) {
W_ _c1ej;
FB_
_c1ej = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1ej;
*Sp = (W_)&s1dK_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1el;
JMP_(*R1.p);
_c1el:
JMP_((W_)&s1dK_info);
FE_
}

static StgWord s1dM_info[] = {
0x2UL, 0x13UL
};

II_(s1dJ_info);
IF_(s1dM_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1eo;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dJ_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1er;
JMP_(*R1.p);
_c1eo:
JMP_(stg_gc_enter_1);
_c1er:
JMP_((W_)&s1dJ_info);
FE_
}

static StgWord s1dG_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1dG_ret) {
W_ _s1dH;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1eG;
_s1dH = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dH;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1eG:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dF_info[] = {
0x1UL, 0x22UL
};

II_(s1dG_info);
IF_(s1dF_ret) {
W_ _c1eK;
FB_
_c1eK = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1eK;
*Sp = (W_)&s1dG_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1eM;
JMP_(*R1.p);
_c1eM:
JMP_((W_)&s1dG_info);
FE_
}

static StgWord s1dI_info[] = {
0x2UL, 0x13UL
};

II_(s1dF_info);
IF_(s1dI_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1eP;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dF_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1eS;
JMP_(*R1.p);
_c1eP:
JMP_(stg_gc_enter_1);
_c1eS:
JMP_((W_)&s1dF_info);
FE_
}

static StgWord s1dE_info[] = {
0x208UL, 0x22UL
};

II_(s1dI_info);
II_(s1dM_info);
IF_(s1dE_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1eV;
case 0x1UL: goto _c1eX;
case 0x2UL: goto _c1eZ;
}
_c1eV:
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1eX:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1f1;
Hp[-3] = (W_)&s1dM_info;
Hp[-1] = Sp[3];
*Hp = Sp[1];
R1.p=Hp-3;
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1f1:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c1eZ:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1f3;
Hp[-3] = (W_)&s1dI_info;
Hp[-1] = Sp[2];
*Hp = Sp[1];
R1.w = Sp[3];
R2.p=Hp-3;
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1f3:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dB_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1dB_ret) {
W_ _s1dC;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1fm;
_s1dC = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dC;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1fm:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dA_info[] = {
0x1UL, 0x22UL
};

II_(s1dB_info);
IF_(s1dA_ret) {
W_ _c1fq;
FB_
_c1fq = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1fq;
*Sp = (W_)&s1dB_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1fs;
JMP_(*R1.p);
_c1fs:
JMP_((W_)&s1dB_info);
FE_
}

static StgWord s1dD_info[] = {
0x2UL, 0x13UL
};

II_(s1dA_info);
IF_(s1dD_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1fv;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dA_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1fy;
JMP_(*R1.p);
_c1fv:
JMP_(stg_gc_enter_1);
_c1fy:
JMP_((W_)&s1dA_info);
FE_
}

static StgWord s1dx_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1dx_ret) {
W_ _s1dy;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1fO;
_s1dy = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dy;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1fO:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dw_info[] = {
0x1UL, 0x22UL
};

II_(s1dx_info);
IF_(s1dw_ret) {
W_ _c1fS;
FB_
_c1fS = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1fS;
*Sp = (W_)&s1dx_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1fU;
JMP_(*R1.p);
_c1fU:
JMP_((W_)&s1dx_info);
FE_
}

static StgWord s1dz_info[] = {
0x2UL, 0x13UL
};

II_(s1dw_info);
IF_(s1dz_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1fX;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dw_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1g0;
JMP_(*R1.p);
_c1fX:
JMP_(stg_gc_enter_1);
_c1g0:
JMP_((W_)&s1dw_info);
FE_
}

static StgWord s1dv_info[] = {
0x208UL, 0x22UL
};

II_(s1dz_info);
II_(s1dD_info);
IF_(s1dv_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1g3;
case 0x1UL: goto _c1g5;
case 0x2UL: goto _c1g7;
}
_c1g3:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1g9;
Hp[-3] = (W_)&s1dD_info;
Hp[-1] = Sp[5];
*Hp = Sp[1];
R1.w = Sp[3];
R2.w = Sp[2];
R3.p=Hp-3;
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1g9:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c1g5:
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1g7:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1gb;
Hp[-3] = (W_)&s1dz_info;
Hp[-1] = Sp[6];
*Hp = Sp[1];
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.p=Hp-3;
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1gb:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1ds_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1ds_ret) {
W_ _s1dt;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1gu;
_s1dt = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dt;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1gu:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dr_info[] = {
0x1UL, 0x22UL
};

II_(s1ds_info);
IF_(s1dr_ret) {
W_ _c1gy;
FB_
_c1gy = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1gy;
*Sp = (W_)&s1ds_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1gA;
JMP_(*R1.p);
_c1gA:
JMP_((W_)&s1ds_info);
FE_
}

static StgWord s1du_info[] = {
0x2UL, 0x13UL
};

II_(s1dr_info);
IF_(s1du_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1gD;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dr_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1gG;
JMP_(*R1.p);
_c1gD:
JMP_(stg_gc_enter_1);
_c1gG:
JMP_((W_)&s1dr_info);
FE_
}

static StgWord s1do_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_Izh_con_info);
IF_(s1do_ret) {
W_ _s1dp;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1gV;
_s1dp = (Sp[1]) + (*((P_)(R1.w+7)));
Hp[-1] = (W_)&ghczmprim_GHCziTypes_Izh_con_info;
*Hp = _s1dp;
R1.w = (W_)Hp-7;
Sp=Sp+2;
JMP_((W_)&stg_upd_frame_info);
_c1gV:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dn_info[] = {
0x1UL, 0x22UL
};

II_(s1do_info);
IF_(s1dn_ret) {
W_ _c1gZ;
FB_
_c1gZ = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1gZ;
*Sp = (W_)&s1do_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1h1;
JMP_(*R1.p);
_c1h1:
JMP_((W_)&s1do_info);
FE_
}

static StgWord s1dq_info[] = {
0x2UL, 0x13UL
};

II_(s1dn_info);
IF_(s1dq_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1h4;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1dn_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1h7;
JMP_(*R1.p);
_c1h4:
JMP_(stg_gc_enter_1);
_c1h7:
JMP_((W_)&s1dn_info);
FE_
}

static StgWord s1dm_info[] = {
0x208UL, 0x22UL
};

II_(s1dq_info);
II_(s1du_info);
IF_(s1dm_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1ha;
case 0x1UL: goto _c1hc;
case 0x2UL: goto _c1he;
}
_c1ha:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1hg;
Hp[-3] = (W_)&s1du_info;
Hp[-1] = Sp[7];
*Hp = Sp[1];
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.p=Hp-3;
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
_c1hg:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c1hc:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1hi;
Hp[-3] = (W_)&s1dq_info;
Hp[-1] = Sp[8];
*Hp = Sp[1];
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.p=Hp-3;
Sp=Sp+9;
JMP_(*Sp);
_c1hi:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c1he:
R1.w = Sp[3];
R2.w = Sp[2];
R3.w = Sp[5];
R4.w = Sp[6];
R5.w = Sp[7];
R6.w = Sp[8];
Sp=Sp+9;
JMP_(*Sp);
FE_
}

static StgWord s1dl_info[] = {
0x8UL, 0x22UL
};

II_(s1dm_info);
II_(s1dv_info);
II_(s1dE_info);
IF_(s1dl_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1hl;
case 0x1UL: goto _c1hn;
case 0x2UL: goto _c1hp;
}
_c1hl:
R1.w = Sp[4];
*Sp = (W_)&s1dE_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1hs;
JMP_(*R1.p);
_c1hs:
JMP_((W_)&s1dE_info);
_c1hn:
R1.w = Sp[4];
*Sp = (W_)&s1dv_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1hv;
JMP_(*R1.p);
_c1hv:
JMP_((W_)&s1dv_info);
_c1hp:
R1.w = Sp[4];
*Sp = (W_)&s1dm_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1hy;
JMP_(*R1.p);
_c1hy:
JMP_((W_)&s1dm_info);
FE_
}

EF_(Aufgabe3ziDatatypes_zdwtourAnwenden_slow);
StgWord Aufgabe3ziDatatypes_zdwtourAnwenden_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtourAnwenden_slow+0), 0x9UL, 0x0, 0x900000000UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdwtourAnwenden_closure);
II_(s1dl_info);
FN_(Aufgabe3ziDatatypes_zdwtourAnwenden_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c1hB;
Sp[-4] = R4.w;
Sp[-3] = R6.w;
Sp[-2] = R5.w;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-5] = (W_)&s1dl_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1hE;
JMP_(*R1.p);
_c1hB:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwtourAnwenden_closure;
Sp=Sp-5;
*Sp = R2.w;
Sp[1] = R3.w;
Sp[2] = R4.w;
Sp[3] = R5.w;
Sp[4] = R6.w;
JMP_(stg_gc_fun);
_c1hE:
JMP_((W_)&s1dl_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwzeze1_info);
StgWord Aufgabe3ziDatatypes_zdwzeze1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwzeze1_info
};


EI_(Aufgabe3ziDatatypes_zdwzeze1_info);
FN_(Aufgabe3ziDatatypes_zdwzeze1_slow) {
FB_
R2.w = *Sp;
R3.w = Sp[1];
R4.w = Sp[2];
R5.w = Sp[3];
R6.w = Sp[4];
Sp=Sp+5;
JMP_((W_)&Aufgabe3ziDatatypes_zdwzeze1_info);
FE_
}

static StgWord s1hX_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_Bool_closure_tbl);
IF_(s1hX_ret) {
W_ _c1jd;
FB_
_c1jd = (W_)((Sp[1]) == (*((P_)(R1.w+7))));
R1.w = *((P_)((W_)&ghczmprim_GHCziBool_Bool_closure_tbl + (_c1jd << 0x3UL)));
Sp=Sp+2;
JMP_(*Sp);
FE_
}

static StgWord s1hW_info[] = {
0x1ff8bUL, 0x22UL
};

II_(s1hX_info);
IF_(s1hW_ret) {
FB_
Sp[11] = *((P_)(R1.w+7));
R1.w = Sp[1];
Sp[10] = (W_)&s1hX_info;
Sp=Sp+10;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jh;
JMP_(*R1.p);
_c1jh:
JMP_((W_)&s1hX_info);
FE_
}

static StgWord s1hU_info[] = {
0x17f8bUL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1hW_info);
IF_(s1hU_ret) {
W_ _c1jj;
FB_
_c1jj = (W_)((Sp[11]) == (*((P_)(R1.w+7))));
if ((W_)(_c1jj >= 0x1UL)) goto _c1jl;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+12;
JMP_(*Sp);
_c1jl:
R1.w = Sp[10];
*Sp = (W_)&s1hW_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jo;
JMP_(*R1.p);
_c1jo:
JMP_((W_)&s1hW_info);
FE_
}

static StgWord s1hT_info[] = {
0x17f0bUL, 0x22UL
};

II_(s1hU_info);
IF_(s1hT_ret) {
FB_
Sp[11] = *((P_)(R1.w+7));
R1.w = Sp[2];
*Sp = (W_)&s1hU_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1js;
JMP_(*R1.p);
_c1js:
JMP_((W_)&s1hU_info);
FE_
}

static StgWord s1hR_info[] = {
0x1770bUL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1hT_info);
IF_(s1hR_ret) {
W_ _c1ju;
FB_
_c1ju = (W_)((Sp[11]) == (*((P_)(R1.w+7))));
if ((W_)(_c1ju >= 0x1UL)) goto _c1jw;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+12;
JMP_(*Sp);
_c1jw:
R1.w = Sp[6];
*Sp = (W_)&s1hT_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jz;
JMP_(*R1.p);
_c1jz:
JMP_((W_)&s1hT_info);
FE_
}

static StgWord s1hQ_info[] = {
0x1760bUL, 0x22UL
};

II_(s1hR_info);
IF_(s1hQ_ret) {
FB_
Sp[11] = *((P_)(R1.w+7));
R1.w = Sp[3];
*Sp = (W_)&s1hR_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jD;
JMP_(*R1.p);
_c1jD:
JMP_((W_)&s1hR_info);
FE_
}

static StgWord s1hO_info[] = {
0x1660bUL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1hQ_info);
IF_(s1hO_ret) {
W_ _c1jF;
FB_
_c1jF = (W_)((Sp[11]) == (*((P_)(R1.w+7))));
if ((W_)(_c1jF >= 0x1UL)) goto _c1jH;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+12;
JMP_(*Sp);
_c1jH:
R1.w = Sp[7];
*Sp = (W_)&s1hQ_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jK;
JMP_(*R1.p);
_c1jK:
JMP_((W_)&s1hQ_info);
FE_
}

static StgWord s1hN_info[] = {
0x1640bUL, 0x22UL
};

II_(s1hO_info);
IF_(s1hN_ret) {
FB_
Sp[11] = *((P_)(R1.w+7));
R1.w = Sp[4];
*Sp = (W_)&s1hO_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jO;
JMP_(*R1.p);
_c1jO:
JMP_((W_)&s1hO_info);
FE_
}

static StgWord s1hL_info[] = {
0x1440bUL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1hN_info);
IF_(s1hL_ret) {
W_ _c1jQ;
FB_
_c1jQ = (W_)((Sp[11]) == (*((P_)(R1.w+7))));
if ((W_)(_c1jQ >= 0x1UL)) goto _c1jS;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+12;
JMP_(*Sp);
_c1jS:
R1.w = Sp[8];
*Sp = (W_)&s1hN_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jV;
JMP_(*R1.p);
_c1jV:
JMP_((W_)&s1hN_info);
FE_
}

static StgWord s1hK_info[] = {
0x1400bUL, 0x22UL
};

II_(s1hL_info);
IF_(s1hK_ret) {
FB_
Sp[11] = *((P_)(R1.w+7));
R1.w = Sp[5];
*Sp = (W_)&s1hL_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1jZ;
JMP_(*R1.p);
_c1jZ:
JMP_((W_)&s1hL_info);
FE_
}

static StgWord s1hI_info[] = {
0x1000bUL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1hK_info);
IF_(s1hI_ret) {
W_ _c1k1;
FB_
_c1k1 = (W_)((Sp[11]) == (*((P_)(R1.w+7))));
if ((W_)(_c1k1 >= 0x1UL)) goto _c1k3;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+12;
JMP_(*Sp);
_c1k3:
R1.w = Sp[9];
*Sp = (W_)&s1hK_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1k6;
JMP_(*R1.p);
_c1k6:
JMP_((W_)&s1hK_info);
FE_
}

static StgWord s1hH_info[] = {
0x806UL, 0x22UL
};

II_(s1hI_info);
IF_(s1hH_ret) {
FB_
Sp[-4] = *((P_)(R1.w+47));
Sp[-3] = *((P_)(R1.w+39));
Sp[-2] = *((P_)(R1.w+31));
Sp[-1] = *((P_)(R1.w+23));
*Sp = *((P_)(R1.w+15));
R1.w = *((P_)(R1.w+7));
Sp[-5] = (W_)&s1hI_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1ka;
JMP_(*R1.p);
_c1ka:
JMP_((W_)&s1hI_info);
FE_
}

EF_(Aufgabe3ziDatatypes_zdwzeze1_slow);
StgWord Aufgabe3ziDatatypes_zdwzeze1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwzeze1_slow+0), 0x47UL, 0x0, 0x700000000UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdwzeze1_closure);
II_(s1hH_info);
FN_(Aufgabe3ziDatatypes_zdwzeze1_entry) {
FB_
if ((W_)(((W_)Sp - 0x50UL) < (W_)SpLim)) goto _c1kd;
Sp[-4] = R6.w;
Sp[-3] = R5.w;
Sp[-2] = R4.w;
Sp[-1] = R3.w;
R1.w = Sp[1];
Sp[1] = R2.w;
Sp[-5] = (W_)&s1hH_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1kg;
JMP_(*R1.p);
_c1kd:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwzeze1_closure;
Sp=Sp-5;
*Sp = R2.w;
Sp[1] = R3.w;
Sp[2] = R4.w;
Sp[3] = R5.w;
Sp[4] = R6.w;
JMP_(stg_gc_fun);
_c1kg:
JMP_((W_)&s1hH_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwzeze_info);
StgWord Aufgabe3ziDatatypes_zdwzeze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwzeze_info
};

static StgWord s1ks_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_Bool_closure_tbl);
IF_(s1ks_ret) {
W_ _c1kX;
FB_
_c1kX = (W_)((Sp[1]) == (*((P_)(R1.w+7))));
R1.w = *((P_)((W_)&ghczmprim_GHCziBool_Bool_closure_tbl + (_c1kX << 0x3UL)));
Sp=Sp+2;
JMP_(*Sp);
FE_
}

static StgWord s1kr_info[] = {
0x1UL, 0x22UL
};

II_(s1ks_info);
IF_(s1kr_ret) {
W_ _c1l1;
FB_
_c1l1 = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1l1;
*Sp = (W_)&s1ks_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1l3;
JMP_(*R1.p);
_c1l3:
JMP_((W_)&s1ks_info);
FE_
}

static StgWord s1kq_info[] = {
0x184UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1kr_info);
IF_(s1kq_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1l6;
case 0x1UL: goto _c1l8;
case 0x2UL: goto _c1la;
}
_c1l6:
R1.w = Sp[1];
Sp[3] = (W_)&s1kr_info;
Sp=Sp+3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1ld;
JMP_(*R1.p);
_c1ld:
JMP_((W_)&s1kr_info);
_c1l8:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
_c1la:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
FE_
}

static StgWord s1kp_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_Bool_closure_tbl);
IF_(s1kp_ret) {
W_ _c1lv;
FB_
_c1lv = (W_)((Sp[1]) == (*((P_)(R1.w+7))));
R1.w = *((P_)((W_)&ghczmprim_GHCziBool_Bool_closure_tbl + (_c1lv << 0x3UL)));
Sp=Sp+2;
JMP_(*Sp);
FE_
}

static StgWord s1ko_info[] = {
0x1UL, 0x22UL
};

II_(s1kp_info);
IF_(s1ko_ret) {
W_ _c1lz;
FB_
_c1lz = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1lz;
*Sp = (W_)&s1kp_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1lB;
JMP_(*R1.p);
_c1lB:
JMP_((W_)&s1kp_info);
FE_
}

static StgWord s1kn_info[] = {
0x184UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1ko_info);
IF_(s1kn_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c1lE;
R1.w = Sp[1];
Sp[3] = (W_)&s1ko_info;
Sp=Sp+3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1lH;
JMP_(*R1.p);
_c1lE:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
_c1lH:
JMP_((W_)&s1ko_info);
FE_
}

static StgWord s1km_info[] = {
0x41UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_Bool_closure_tbl);
IF_(s1km_ret) {
W_ _c1lZ;
FB_
_c1lZ = (W_)((Sp[1]) == (*((P_)(R1.w+7))));
R1.w = *((P_)((W_)&ghczmprim_GHCziBool_Bool_closure_tbl + (_c1lZ << 0x3UL)));
Sp=Sp+2;
JMP_(*Sp);
FE_
}

static StgWord s1kl_info[] = {
0x1UL, 0x22UL
};

II_(s1km_info);
IF_(s1kl_ret) {
W_ _c1m3;
FB_
_c1m3 = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1m3;
*Sp = (W_)&s1km_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1m5;
JMP_(*R1.p);
_c1m5:
JMP_((W_)&s1km_info);
FE_
}

static StgWord s1kk_info[] = {
0x184UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(s1kl_info);
IF_(s1kk_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1m8;
R1.w = Sp[1];
Sp[3] = (W_)&s1kl_info;
Sp=Sp+3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1mb;
JMP_(*R1.p);
_c1m8:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
_c1mb:
JMP_((W_)&s1kl_info);
FE_
}

static StgWord s1kj_info[] = {
0x104UL, 0x22UL
};

II_(s1kk_info);
II_(s1kn_info);
II_(s1kq_info);
IF_(s1kj_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1me;
case 0x1UL: goto _c1mg;
case 0x2UL: goto _c1mi;
}
_c1me:
R1.w = Sp[2];
*Sp = (W_)&s1kq_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1ml;
JMP_(*R1.p);
_c1ml:
JMP_((W_)&s1kq_info);
_c1mg:
R1.w = Sp[2];
*Sp = (W_)&s1kn_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1mo;
JMP_(*R1.p);
_c1mo:
JMP_((W_)&s1kn_info);
_c1mi:
R1.w = Sp[2];
*Sp = (W_)&s1kk_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1mr;
JMP_(*R1.p);
_c1mr:
JMP_((W_)&s1kk_info);
FE_
}

static StgWord sY4_info[] = {
0x4UL, 0x22UL
};

II_(s1kj_info);
IF_(sY4_ret) {
FB_
R1.w = Sp[3];
*Sp = (W_)&s1kj_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1mv;
JMP_(*R1.p);
_c1mv:
JMP_((W_)&s1kj_info);
FE_
}

static StgWord s1kw_info[] = {
0x4UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(sY4_info);
IF_(s1kw_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1mE;
case 0x1UL: goto _c1mG;
case 0x2UL: goto _c1mI;
}
_c1mE:
JMP_((W_)&sY4_info);
_c1mG:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
_c1mI:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
FE_
}

static StgWord s1kv_info[] = {
0x4UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(sY4_info);
IF_(s1kv_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c1mP;
JMP_((W_)&sY4_info);
_c1mP:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
FE_
}

static StgWord s1ku_info[] = {
0x4UL, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
II_(sY4_info);
IF_(s1ku_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1mW;
JMP_((W_)&sY4_info);
_c1mW:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+5;
JMP_(*Sp);
FE_
}

static StgWord s1kt_info[] = {
0x5UL, 0x22UL
};

II_(s1ku_info);
II_(s1kv_info);
II_(s1kw_info);
IF_(s1kt_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1mZ;
case 0x1UL: goto _c1n1;
case 0x2UL: goto _c1n3;
}
_c1mZ:
R1.w = Sp[1];
Sp[1] = (W_)&s1kw_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1n6;
JMP_(*R1.p);
_c1n6:
JMP_((W_)&s1kw_info);
_c1n1:
R1.w = Sp[1];
Sp[1] = (W_)&s1kv_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1n9;
JMP_(*R1.p);
_c1n9:
JMP_((W_)&s1kv_info);
_c1n3:
R1.w = Sp[1];
Sp[1] = (W_)&s1ku_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1nc;
JMP_(*R1.p);
_c1nc:
JMP_((W_)&s1ku_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdwzeze_info[] = {
0x600000017UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdwzeze_closure);
II_(s1kt_info);
FN_(Aufgabe3ziDatatypes_zdwzeze_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c1nf;
Sp[-1] = R3.w;
Sp[-2] = R6.w;
Sp[-3] = R4.w;
Sp[-4] = R5.w;
R1.p=R2.p;
Sp[-5] = (W_)&s1kt_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1ni;
JMP_(*R1.p);
_c1nf:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwzeze_closure;
JMP_(stg_gc_fun);
_c1ni:
JMP_((W_)&s1kt_info);
FE_
}
EI_(ghczmprim_GHCziTypes_Czh_static_info);
static StgWord rUK_closure[] = {
(W_)&ghczmprim_GHCziTypes_Czh_static_info, 0x7dUL
};
EI_(ghczmprim_GHCziTypes_ZC_static_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(rUK_closure);
static StgWord rUM_closure[] = {
(W_)&ghczmprim_GHCziTypes_ZC_static_info, ((W_)&rUK_closure+1), ((W_)&ghczmprim_GHCziTypes_ZMZN_closure+1), 0x1UL
};
II_(rUO_info);
static StgWord rUO_closure[] = {
(W_)&rUO_info, 0x0, 0x0, 0x0
};

static char c1nz_str[] = "cZuB = ";

static StgWord rUO_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUO_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1nC;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1nC;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1nz_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1nC:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUQ_info);
static StgWord rUQ_closure[] = {
(W_)&rUQ_info, 0x0, 0x0, 0x0
};

static char c1nL_str[] = ", ";

static StgWord rUQ_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUQ_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1nO;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1nO;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1nL_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1nO:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUS_info);
static StgWord rUS_closure[] = {
(W_)&rUS_info, 0x0, 0x0, 0x0
};

static char c1nX_str[] = "cZuA = ";

static StgWord rUS_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUS_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1o0;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1o0;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1nX_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1o0:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUU_info);
static StgWord rUU_closure[] = {
(W_)&rUU_info, 0x0, 0x0, 0x0
};

static char c1o9_str[] = "bZuC = ";

static StgWord rUU_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUU_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1oc;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1oc;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1o9_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1oc:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUW_info);
static StgWord rUW_closure[] = {
(W_)&rUW_info, 0x0, 0x0, 0x0
};

static char c1ol_str[] = "bZuA = ";

static StgWord rUW_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUW_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1oo;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1oo;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1ol_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1oo:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUY_info);
static StgWord rUY_closure[] = {
(W_)&rUY_info, 0x0, 0x0, 0x0
};

static char c1ox_str[] = "aZuC = ";

static StgWord rUY_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rUY_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1oA;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1oA;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1ox_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1oA:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rV0_info);
static StgWord rV0_closure[] = {
(W_)&rV0_info, 0x0, 0x0, 0x0
};

static char c1oJ_str[] = "aZuB = ";

static StgWord rV0_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rV0_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1oM;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1oM;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1oJ_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1oM:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rV2_info);
static StgWord rV2_closure[] = {
(W_)&rV2_info, 0x0, 0x0, 0x0
};

static char c1oV_str[] = "TourSet {";

static StgWord rV2_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rV2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1oY;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1oY;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1oV_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1oY:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rUO_closure);
II_(rUQ_closure);
II_(rUS_closure);
II_(rUU_closure);
II_(rUW_closure);
II_(rUY_closure);
II_(rV0_closure);
II_(rV2_closure);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec1_srt[] = {
(W_)&rUO_closure, (W_)&rUQ_closure, (W_)&rUS_closure, (W_)&rUU_closure, (W_)&rUW_closure, (W_)&rUY_closure, (W_)&rV0_closure, (W_)&rV2_closure
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_info);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_info, 0x0
};


EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_info);
FN_(Aufgabe3ziDatatypes_zdwshowsPrec1_slow) {
FB_
R2.w = *Sp;
R3.w = Sp[1];
R4.w = Sp[2];
R5.w = Sp[3];
R6.w = Sp[4];
Sp=Sp+5;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_info);
FE_
}

static StgWord s1p7_info[] = {
0x1UL, 0x11UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUM_closure);
IF_(s1p7_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1qC;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = (W_)&rUM_closure+2;
R3.w = R1.p[2];
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1qC:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p6_info[] = {
0x1UL, 0x22UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1p7_info);
IF_(s1p6_ret) {
FB_
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1qF;
Hp[-2] = (W_)&s1p7_info;
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-2;
Sp=Sp+2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1qF:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p8_info[] = {
0x2UL, 0x13UL
};

II_(s1p6_info);
IF_(s1p8_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1qI;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1p6_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1qL;
JMP_(*R1.p);
_c1qI:
JMP_(stg_gc_enter_1);
_c1qL:
JMP_((W_)&s1p6_info);
FE_
}

static StgWord s1p9_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x2UL, 0x100000013UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUO_closure);
II_(s1p8_info);
IF_(s1p9_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1qO;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1qO;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1p8_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
R2.w = (W_)&rUO_closure;
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1qO:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pa_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x2UL, 0x300000013UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1p9_info);
IF_(s1pa_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1qR;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1qR;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1p9_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1qR:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p5_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x2UL, 0x300000022UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1pa_info);
IF_(s1p5_ret) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1qU;
Hp[-3] = (W_)&s1pa_info;
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1qU:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pb_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x3UL, 0x300000010UL
};

II_(s1p5_info);
IF_(s1pb_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c1qX;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-5] = (W_)&s1p5_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1r0;
JMP_(*R1.p);
_c1qX:
JMP_(stg_gc_enter_1);
_c1r0:
JMP_((W_)&s1p5_info);
FE_
}

static StgWord s1pc_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x3UL, 0x700000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUS_closure);
II_(s1pb_info);
IF_(s1pc_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1r3;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1r3;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&s1pb_info;
Hp[-2] = R1.p[2];
Hp[-1] = R1.p[3];
*Hp = R1.p[4];
R2.w = (W_)&rUS_closure;
R3.p=Hp-4;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1r3:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pd_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x3UL, 0x700000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1pc_info);
IF_(s1pd_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1r6;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1r6;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&s1pc_info;
Hp[-2] = R1.p[2];
Hp[-1] = R1.p[3];
*Hp = R1.p[4];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-4;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1r6:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p4_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x3UL, 0x700000022UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1pd_info);
IF_(s1p4_ret) {
FB_
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1r9;
Hp[-4] = (W_)&s1pd_info;
Hp[-2] = Sp[3];
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-4;
Sp=Sp+4;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1r9:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pe_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x4UL, 0x700000010UL
};

II_(s1p4_info);
IF_(s1pe_entry) {
FB_
if ((W_)(((W_)Sp - 0x30UL) < (W_)SpLim)) goto _c1rc;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-5] = R1.p[5];
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-6] = (W_)&s1p4_info;
Sp=Sp-6;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1rf;
JMP_(*R1.p);
_c1rc:
JMP_(stg_gc_enter_1);
_c1rf:
JMP_((W_)&s1p4_info);
FE_
}

static StgWord s1pf_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x4UL, 0xf00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUU_closure);
II_(s1pe_info);
IF_(s1pf_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1ri;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1ri;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&s1pe_info;
Hp[-3] = R1.p[2];
Hp[-2] = R1.p[3];
Hp[-1] = R1.p[4];
*Hp = R1.p[5];
R2.w = (W_)&rUU_closure;
R3.p=Hp-5;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1ri:
HpAlloc = 0x30UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pg_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x4UL, 0xf00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1pf_info);
IF_(s1pg_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1rl;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rl;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&s1pf_info;
Hp[-3] = R1.p[2];
Hp[-2] = R1.p[3];
Hp[-1] = R1.p[4];
*Hp = R1.p[5];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-5;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1rl:
HpAlloc = 0x30UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p3_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x4UL, 0xf00000022UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1pg_info);
IF_(s1p3_ret) {
FB_
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1ro;
Hp[-5] = (W_)&s1pg_info;
Hp[-3] = Sp[4];
Hp[-2] = Sp[3];
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-5;
Sp=Sp+5;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1ro:
HpAlloc = 0x30UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1ph_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x5UL, 0xf00000010UL
};

II_(s1p3_info);
IF_(s1ph_entry) {
FB_
if ((W_)(((W_)Sp - 0x38UL) < (W_)SpLim)) goto _c1rr;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-6] = R1.p[6];
Sp[-5] = R1.p[5];
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-7] = (W_)&s1p3_info;
Sp=Sp-7;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1ru;
JMP_(*R1.p);
_c1rr:
JMP_(stg_gc_enter_1);
_c1ru:
JMP_((W_)&s1p3_info);
FE_
}

static StgWord s1pi_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x5UL, 0x1f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUW_closure);
II_(s1ph_info);
IF_(s1pi_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1rx;
Hp=Hp+7;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rx;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-6] = (W_)&s1ph_info;
Hp[-4] = R1.p[2];
Hp[-3] = R1.p[3];
Hp[-2] = R1.p[4];
Hp[-1] = R1.p[5];
*Hp = R1.p[6];
R2.w = (W_)&rUW_closure;
R3.p=Hp-6;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1rx:
HpAlloc = 0x38UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pj_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x5UL, 0x1f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1pi_info);
IF_(s1pj_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1rA;
Hp=Hp+7;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rA;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-6] = (W_)&s1pi_info;
Hp[-4] = R1.p[2];
Hp[-3] = R1.p[3];
Hp[-2] = R1.p[4];
Hp[-1] = R1.p[5];
*Hp = R1.p[6];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-6;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1rA:
HpAlloc = 0x38UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p2_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x5UL, 0x1f00000022UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1pj_info);
IF_(s1p2_ret) {
FB_
Hp=Hp+7;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rD;
Hp[-6] = (W_)&s1pj_info;
Hp[-4] = Sp[5];
Hp[-3] = Sp[4];
Hp[-2] = Sp[3];
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-6;
Sp=Sp+6;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1rD:
HpAlloc = 0x38UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pk_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x6UL, 0x1f00000010UL
};

II_(s1p2_info);
IF_(s1pk_entry) {
FB_
if ((W_)(((W_)Sp - 0x40UL) < (W_)SpLim)) goto _c1rG;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-7] = R1.p[7];
Sp[-6] = R1.p[6];
Sp[-5] = R1.p[5];
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-8] = (W_)&s1p2_info;
Sp=Sp-8;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1rJ;
JMP_(*R1.p);
_c1rG:
JMP_(stg_gc_enter_1);
_c1rJ:
JMP_((W_)&s1p2_info);
FE_
}

static StgWord s1pl_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x6UL, 0x3f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUY_closure);
II_(s1pk_info);
IF_(s1pl_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1rM;
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rM;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-7] = (W_)&s1pk_info;
Hp[-5] = R1.p[2];
Hp[-4] = R1.p[3];
Hp[-3] = R1.p[4];
Hp[-2] = R1.p[5];
Hp[-1] = R1.p[6];
*Hp = R1.p[7];
R2.w = (W_)&rUY_closure;
R3.p=Hp-7;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1rM:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pm_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x6UL, 0x3f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1pl_info);
IF_(s1pm_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1rP;
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rP;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-7] = (W_)&s1pl_info;
Hp[-5] = R1.p[2];
Hp[-4] = R1.p[3];
Hp[-3] = R1.p[4];
Hp[-2] = R1.p[5];
Hp[-1] = R1.p[6];
*Hp = R1.p[7];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-7;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1rP:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1p1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x6UL, 0x3f00000022UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1pm_info);
IF_(s1p1_ret) {
FB_
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1rS;
Hp[-7] = (W_)&s1pm_info;
Hp[-5] = Sp[6];
Hp[-4] = Sp[5];
Hp[-3] = Sp[4];
Hp[-2] = Sp[3];
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-7;
Sp=Sp+7;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1rS:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pn_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x7UL, 0x3f00000010UL
};

II_(s1p1_info);
IF_(s1pn_entry) {
FB_
if ((W_)(((W_)Sp - 0x48UL) < (W_)SpLim)) goto _c1rV;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-8] = R1.p[8];
Sp[-7] = R1.p[7];
Sp[-6] = R1.p[6];
Sp[-5] = R1.p[5];
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-9] = (W_)&s1p1_info;
Sp=Sp-9;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1rY;
JMP_(*R1.p);
_c1rV:
JMP_(stg_gc_enter_1);
_c1rY:
JMP_((W_)&s1p1_info);
FE_
}

static StgWord sZ1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x100000005UL, 0x6UL, 0x7f00000009UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rV0_closure);
II_(s1pn_info);
IF_(sZ1_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1s1;
Hp[-8] = (W_)&s1pn_info;
Hp[-6] = *((P_)(R1.w+7));
Hp[-5] = *((P_)(R1.w+15));
Hp[-4] = *((P_)(R1.w+23));
Hp[-3] = *((P_)(R1.w+31));
Hp[-2] = *((P_)(R1.w+39));
Hp[-1] = *((P_)(R1.w+47));
*Hp = R2.w;
R2.w = (W_)&rV0_closure;
R3.p=Hp-8;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1s1:
HpAlloc = 0x48UL;
JMP_(stg_gc_fun);
FE_
}

static StgWord s1pq_info[] = {
0x2UL, 0x13UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showSignedInt1_closure);
II_(sZ1_info);
IF_(s1pq_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1sc;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sc;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showSignedInt1_closure;
*Hp = R1.p[3];
R1.w = R1.p[2];
R2.w = (W_)Hp-14;
Sp=Sp-2;
JMP_((W_)&sZ1_info);
_c1sc:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1pr_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+56), 0x2UL, 0x100000013UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rV2_closure);
II_(s1pq_info);
IF_(s1pr_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1sf;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sf;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1pq_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
R2.w = (W_)&rV2_closure;
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1sf:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

EF_(Aufgabe3ziDatatypes_zdwshowsPrec1_slow);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_slow+0), 0x48UL, ((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_srt+0), 0x800000000UL, 0x0, 0xff0000000fUL
};

EI_(base_GHCziBase_zpzp_info);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_closure);
EI_(base_GHCziShow_showSignedInt2_closure);
II_(rV2_closure);
II_(sZ1_info);
II_(s1pr_info);
FN_(Aufgabe3ziDatatypes_zdwshowsPrec1_entry) {
W_ _c1sh;
FB_
Hp=Hp+14;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sj;
Hp[-13] = (W_)&sZ1_info;
Hp[-12] = R3.w;
Hp[-11] = R4.w;
Hp[-10] = R5.w;
Hp[-9] = R6.w;
Hp[-8] = *Sp;
Hp[-7] = Sp[1];
_c1sh = (W_)((I_)R2.w >= (I_)0xbUL);
if ((W_)(_c1sh >= 0x1UL)) goto _c1sl;
Hp[-6] = (W_)&stg_ap_2_upd_info;
Hp[-4] = (W_)Hp-103;
Hp[-3] = Sp[2];
R2.w = (W_)&rV2_closure;
R3.p=Hp-6;
Sp=Sp+3;
Hp=Hp-3;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1sj:
HpAlloc = 0x70UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_closure;
Sp=Sp-5;
*Sp = R2.w;
Sp[1] = R3.w;
Sp[2] = R4.w;
Sp[3] = R5.w;
Sp[4] = R6.w;
JMP_(stg_gc_fun);
_c1sl:
Hp[-6] = (W_)&s1pr_info;
Hp[-4] = (W_)Hp-103;
Hp[-3] = Sp[2];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showSignedInt2_closure;
*Hp = (W_)Hp-48;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
FE_
}
II_(rV4_info);
static StgWord rV4_closure[] = {
(W_)&rV4_info, 0x0, 0x0, 0x0
};

static char c1su_str[] = "fahrten = ";

static StgWord rV4_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rV4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1sx;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sx;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1su_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1sx:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rV6_info);
static StgWord rV6_closure[] = {
(W_)&rV6_info, 0x0, 0x0, 0x0
};

static char c1sG_str[] = "ziel = ";

static StgWord rV6_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rV6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1sJ;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sJ;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1sG_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1sJ:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rV8_info);
static StgWord rV8_closure[] = {
(W_)&rV8_info, 0x0, 0x0, 0x0
};

static char c1sS_str[] = "start = ";

static StgWord rV8_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rV8_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1sV;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1sV;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1sS_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1sV:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rVa_info);
static StgWord rVa_closure[] = {
(W_)&rVa_info, 0x0, 0x0, 0x0
};

static char c1t4_str[] = "Tour {";

static StgWord rVa_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rVa_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1t7;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1t7;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1t4_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1t7:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager1_info);
StgWord Aufgabe3ziDatatypes_zdfShowLager1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager1_info, 0x0, 0x0, 0x0
};

static char c1tg_str[] = "LagerC";

StgWord Aufgabe3ziDatatypes_zdfShowLager1_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowLager1_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1tj;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1tj;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1tg_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1tj:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager2_info);
StgWord Aufgabe3ziDatatypes_zdfShowLager2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager2_info, 0x0, 0x0, 0x0
};

static char c1ts_str[] = "LagerB";

StgWord Aufgabe3ziDatatypes_zdfShowLager2_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowLager2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1tv;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1tv;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1ts_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1tv:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager3_info);
StgWord Aufgabe3ziDatatypes_zdfShowLager3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager3_info, 0x0, 0x0, 0x0
};

static char c1tE_str[] = "LagerA";

StgWord Aufgabe3ziDatatypes_zdfShowLager3_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowLager3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1tH;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1tH;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1tE_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1tH:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
II_(rUQ_closure);
II_(rV4_closure);
II_(rV6_closure);
II_(rV8_closure);
II_(rVa_closure);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure, (W_)&rUQ_closure, (W_)&rV4_closure, (W_)&rV6_closure, (W_)&rV8_closure, (W_)&rVa_closure
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec_info);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec_info, 0x0
};


EI_(Aufgabe3ziDatatypes_zdwshowsPrec_info);
FN_(Aufgabe3ziDatatypes_zdwshowsPrec_slow) {
FB_
R2.w = *Sp;
R3.w = Sp[1];
R4.w = Sp[2];
R5.w = Sp[3];
R6.w = Sp[4];
Sp=Sp+5;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_info);
FE_
}

static StgWord s1tL_info[] = {
0x1UL, 0x11UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUM_closure);
IF_(s1tL_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1uw;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = (W_)&rUM_closure+2;
R3.w = R1.p[2];
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1uw:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1tK_info[] = {
0x1UL, 0x22UL
};

EI_(base_GHCziShow_zdwshowSignedInt_info);
II_(s1tL_info);
IF_(s1tK_ret) {
FB_
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1uz;
Hp[-2] = (W_)&s1tL_info;
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.p=Hp-2;
Sp=Sp+2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1uz:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1tM_info[] = {
0x2UL, 0x13UL
};

II_(s1tK_info);
IF_(s1tM_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1uC;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-4] = (W_)&s1tK_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1uF;
JMP_(*R1.p);
_c1uC:
JMP_(stg_gc_enter_1);
_c1uF:
JMP_((W_)&s1tK_info);
FE_
}

static StgWord s1tN_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+32), 0x2UL, 0x100000013UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rV4_closure);
II_(s1tM_info);
IF_(s1tN_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1uI;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1uI;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1tM_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
R2.w = (W_)&rV4_closure;
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1uI:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sZq_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+24), 0x2UL, 0x300000013UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1tN_info);
IF_(sZq_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1uL;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1uL;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1tN_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1uL:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1tO_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x1UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s1tO_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1uQ;
case 0x1UL: goto _c1uS;
case 0x2UL: goto _c1uU;
}
_c1uQ:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1uS:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1uU:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
FE_
}

static StgWord s1tP_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x3UL, 0x1f00000010UL
};

II_(sZq_info);
II_(s1tO_info);
IF_(s1tP_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1uX;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1uX;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&sZq_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
Sp[-3] = (W_)Hp-24;
R1.w = R1.p[4];
Sp[-4] = (W_)&s1tO_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1v0;
JMP_(*R1.p);
_c1uX:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c1v0:
JMP_((W_)&s1tO_info);
FE_
}

static StgWord s1tQ_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x3UL, 0x3f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rV6_closure);
II_(s1tP_info);
IF_(s1tQ_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1v3;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1v3;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&s1tP_info;
Hp[-2] = R1.p[2];
Hp[-1] = R1.p[3];
*Hp = R1.p[4];
R2.w = (W_)&rV6_closure;
R3.p=Hp-4;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1v3:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sZv_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x3UL, 0x3f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rUQ_closure);
II_(s1tQ_info);
IF_(sZv_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1v6;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1v6;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&s1tQ_info;
Hp[-2] = R1.p[2];
Hp[-1] = R1.p[3];
*Hp = R1.p[4];
R2.w = (W_)&rUQ_closure;
R3.p=Hp-4;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1v6:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1tR_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x1UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s1tR_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1vb;
case 0x1UL: goto _c1vd;
case 0x2UL: goto _c1vf;
}
_c1vb:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1vd:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1vf:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
FE_
}

static StgWord s1tS_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x4UL, 0x3f00000010UL
};

II_(sZv_info);
II_(s1tR_info);
IF_(s1tS_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c1vi;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vi;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&sZv_info;
Hp[-2] = R1.p[2];
Hp[-1] = R1.p[3];
*Hp = R1.p[4];
Sp[-3] = (W_)Hp-32;
R1.w = R1.p[5];
Sp[-4] = (W_)&s1tR_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1vl;
JMP_(*R1.p);
_c1vi:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
_c1vl:
JMP_((W_)&s1tR_info);
FE_
}

static StgWord s1tT_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x4UL, 0x7f00000010UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rV8_closure);
II_(s1tS_info);
IF_(s1tT_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1vo;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vo;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&s1tS_info;
Hp[-3] = R1.p[2];
Hp[-2] = R1.p[3];
Hp[-1] = R1.p[4];
*Hp = R1.p[5];
R2.w = (W_)&rV8_closure;
R3.p=Hp-5;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1vo:
HpAlloc = 0x30UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sZA_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x100000005UL, 0x3UL, 0xff00000009UL
};

EI_(base_GHCziBase_zpzp_info);
II_(rVa_closure);
II_(s1tT_info);
IF_(sZA_entry) {
FB_
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vr;
Hp[-5] = (W_)&s1tT_info;
Hp[-3] = *((P_)(R1.w+7));
Hp[-2] = R2.w;
Hp[-1] = *((P_)(R1.w+15));
*Hp = *((P_)(R1.w+23));
R2.w = (W_)&rVa_closure;
R3.p=Hp-5;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1vr:
HpAlloc = 0x30UL;
JMP_(stg_gc_fun);
FE_
}

static StgWord s1tW_info[] = {
0x2UL, 0x13UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showSignedInt1_closure);
II_(sZA_info);
IF_(s1tW_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1vA;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vA;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showSignedInt1_closure;
*Hp = R1.p[3];
R1.w = R1.p[2];
R2.w = (W_)Hp-14;
Sp=Sp-2;
JMP_((W_)&sZA_info);
_c1vA:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

EF_(Aufgabe3ziDatatypes_zdwshowsPrec_slow);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_slow+0), 0x45UL, ((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_srt+0), 0x500000000UL, 0x0, 0xff0000000fUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdwshowsPrec_closure);
EI_(base_GHCziShow_showSignedInt2_closure);
II_(sZA_info);
II_(s1tW_info);
FN_(Aufgabe3ziDatatypes_zdwshowsPrec_entry) {
W_ _c1vC;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vE;
Hp[-10] = (W_)&sZA_info;
Hp[-9] = R5.w;
Hp[-8] = R4.w;
Hp[-7] = R3.w;
_c1vC = (W_)((I_)R2.w >= (I_)0xbUL);
if ((W_)(_c1vC >= 0x1UL)) goto _c1vG;
R1.w = (W_)Hp-79;
R2.p=R6.p;
Hp=Hp-7;
JMP_((W_)&sZA_info);
_c1vE:
HpAlloc = 0x58UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdwshowsPrec_closure;
Sp=Sp-5;
*Sp = R2.w;
Sp[1] = R3.w;
Sp[2] = R4.w;
Sp[3] = R5.w;
Sp[4] = R6.w;
JMP_(stg_gc_fun);
_c1vG:
Hp[-6] = (W_)&s1tW_info;
Hp[-4] = (W_)Hp-79;
Hp[-3] = R6.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showSignedInt2_closure;
*Hp = (W_)Hp-48;
R1.w = (W_)Hp-14;
JMP_(*Sp);
FE_
}
II_(rVc_info);
static StgWord rVc_closure[] = {
(W_)&rVc_info, 0x0, 0x0, 0x0
};

static char c1vP_str[] = "succ{Lager}: tried to take `succ\' of last tag in enumeration";

static StgWord rVc_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rVc_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1vS;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1vS;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1vP_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1vS:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
II_(rVc_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager12_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&rVc_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager12_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager12_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager12_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager12_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager12_srt+0), 0x0, 0x300000016UL
};

EI_(base_GHCziErr_error_info);
II_(rVc_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumLager12_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1w2;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1w2;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&rVc_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziErr_error_info);
_c1w2:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rVe_info);
static StgWord rVe_closure[] = {
(W_)&rVe_info, 0x0, 0x0, 0x0
};

static char c1wb_str[] = "pred{Lager}: tried to take `pred\' of first tag in enumeration";

static StgWord rVe_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rVe_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1we;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1we;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1wb_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1we:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
II_(rVe_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager11_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&rVe_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager11_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager11_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager11_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager11_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager11_srt+0), 0x0, 0x300000016UL
};

EI_(base_GHCziErr_error_info);
II_(rVe_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumLager11_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1wo;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1wo;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&rVe_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziErr_error_info);
_c1wo:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(ghczmprim_GHCziTypes_Czh_static_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager5_closure[] = {
(W_)&ghczmprim_GHCziTypes_Czh_static_info, 0x29UL
};
EI_(ghczmprim_GHCziTypes_ZC_static_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager5_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager4_closure[] = {
(W_)&ghczmprim_GHCziTypes_ZC_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEnumLager5_closure+1), ((W_)&ghczmprim_GHCziTypes_ZMZN_closure+1), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdfEnumLager3_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager3_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager3_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager4_closure);
EI_(base_GHCziShow_zdwshowSignedInt_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1wF;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1wF;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x0;
R3.w = 0x2UL;
R4.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager4_closure+2;
Sp=Sp-2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1wF:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager3_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager2_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager3_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager2_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager2_info, 0x0, 0x0, 0x0
};

static char c1wP_str[] = ") is outside of enumeration\'s range (0,";

StgWord Aufgabe3ziDatatypes_zdfEnumLager2_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager2_srt+0), 0x0, 0x100000016UL
};

EI_(base_GHCziBase_unpackAppendCStringzh_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLager3_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumLager2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1wS;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1wS;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1wP_str;
R3.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager3_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackAppendCStringzh_info);
_c1wS:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager6_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager6_info, 0x0, 0x0, 0x0
};

static char c1x1_str[] = "toEnum{Lager}: tag (";

StgWord Aufgabe3ziDatatypes_zdfEnumLager6_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1x4;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1x4;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1x1_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1x4:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager6_closure);
StgWord Aufgabe3ziDatatypes_zdwtoEnum_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumLager2_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumLager6_closure
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum_info);
StgWord Aufgabe3ziDatatypes_zdwtoEnum_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum_info, 0x0
};

static StgWord s1x7_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum_srt+8), 0x100000000UL, 0x100000012UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager2_closure);
EI_(base_GHCziShow_zdwshowSignedInt_info);
IF_(s1x7_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1xo;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = 0x0;
R3.w = R1.p[2];
R4.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager2_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1xo:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1x8_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum_srt+0), 0x0, 0x100000022UL
};

EI_(base_GHCziErr_error_info);
IF_(s1x8_ret) {
FB_
R2.p=R1.p;
Sp=Sp+1;
JMP_((W_)&base_GHCziErr_error_info);
FE_
}

static StgWord sZN_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum_srt+0), 0x41UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLager6_closure);
II_(s1x7_info);
II_(sZN_info);
II_(s1x8_info);
IF_(sZN_ret) {
FB_
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1xs;
Hp[-2] = (W_)&s1x7_info;
*Hp = Sp[1];
R2.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager6_closure;
R3.p=Hp-2;
Sp[1] = (W_)&s1x8_info;
Sp=Sp+1;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1xs:
HpAlloc = 0x18UL;
*Sp = (W_)&sZN_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
FE_
}

StgWord Aufgabe3ziDatatypes_zdwtoEnum_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum_srt+0), 0x100000004UL, 0x0, 0x70000000fUL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum_closure);
EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
II_(sZN_info);
FN_(Aufgabe3ziDatatypes_zdwtoEnum_entry) {
W_ _c1xz;
W_ _c1xA;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1xC;
Sp[-1] = R2.w;
_c1xA = (W_)((I_)R2.w >= (I_)0x0);
if ((W_)(_c1xA >= 0x1UL)) goto _c1xE;
Sp=Sp-2;
JMP_((W_)&sZN_info);
_c1xC:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwtoEnum_closure;
JMP_(stg_gc_fun);
_c1xE:
_c1xz = (W_)((I_)R2.w <= (I_)0x2UL);
if ((W_)(_c1xz >= 0x1UL)) goto _c1xH;
Sp=Sp-2;
JMP_((W_)&sZN_info);
_c1xH:
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (R2.w << 0x3UL)));
JMP_(*Sp);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag1_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag1_info, 0x0, 0x0, 0x0
};

static char c1xQ_str[] = "Sonntag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag1_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag1_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1xT;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1xT;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1xQ_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1xT:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag2_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag2_info, 0x0, 0x0, 0x0
};

static char c1y2_str[] = "Samstag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag2_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1y5;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1y5;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1y2_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1y5:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag3_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag3_info, 0x0, 0x0, 0x0
};

static char c1ye_str[] = "Freitag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag3_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1yh;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1yh;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1ye_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1yh:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag4_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag4_info, 0x0, 0x0, 0x0
};

static char c1yq_str[] = "Donnerstag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag4_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1yt;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1yt;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1yq_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1yt:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag5_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag5_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag5_info, 0x0, 0x0, 0x0
};

static char c1yC_str[] = "Mittwoch";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag5_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag5_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1yF;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1yF;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1yC_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1yF:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag6_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag6_info, 0x0, 0x0, 0x0
};

static char c1yO_str[] = "Dienstag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag6_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1yR;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1yR;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1yO_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1yR:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag7_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag7_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag7_info, 0x0, 0x0, 0x0
};

static char c1z0_str[] = "Montag";

StgWord Aufgabe3ziDatatypes_zdfShowWochentag7_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag7_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1z3;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1z3;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1z0_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1z3:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentag1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag3_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag4_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag5_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag6_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag7_closure);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec2_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag1_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag2_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag3_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag4_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag5_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag6_closure, (W_)&Aufgabe3ziDatatypes_zdfShowWochentag7_closure
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_info);
StgWord Aufgabe3ziDatatypes_zdwshowsPrec2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_info, 0x0
};

static StgWord s1z6_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_srt+0), 0x1UL, 0x7f00000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag3_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag4_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag5_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag6_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag7_closure);
IF_(s1z6_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1zh;
case 0x1UL: goto _c1zj;
case 0x2UL: goto _c1zl;
case 0x3UL: goto _c1zn;
case 0x4UL: goto _c1zp;
case 0x5UL: goto _c1zr;
case 0x6UL: goto _c1zt;
}
_c1zh:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag7_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zj:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag6_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zl:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag5_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zn:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag4_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zp:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag3_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zr:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag2_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1zt:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentag1_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdwshowsPrec2_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_srt+0), 0x20000000cUL, 0x0, 0x7f0000000fUL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_closure);
II_(s1z6_info);
FN_(Aufgabe3ziDatatypes_zdwshowsPrec2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1zw;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1z6_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1zz;
JMP_(*R1.p);
_c1zw:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_closure;
JMP_(stg_gc_fun);
_c1zz:
JMP_((W_)&s1z6_info);
FE_
}
II_(rVg_info);
static StgWord rVg_closure[] = {
(W_)&rVg_info, 0x0, 0x0, 0x0
};

static char c1zI_str[] = "succ{Wochentag}: tried to take `succ\' of last tag in enumeration";

static StgWord rVg_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rVg_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1zL;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1zL;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1zI_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1zL:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
II_(rVg_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag14_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&rVg_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag14_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag14_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag14_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag14_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag14_srt+0), 0x0, 0x300000016UL
};

EI_(base_GHCziErr_error_info);
II_(rVg_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag14_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1zV;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1zV;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&rVg_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziErr_error_info);
_c1zV:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
II_(rVi_info);
static StgWord rVi_closure[] = {
(W_)&rVi_info, 0x0, 0x0, 0x0
};

static char c1A4_str[] = "pred{Wochentag}: tried to take `pred\' of first tag in enumeration";

static StgWord rVi_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
IF_(rVi_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1A7;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1A7;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1A4_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1A7:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
II_(rVi_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag13_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&rVi_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag13_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag13_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag13_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag13_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag13_srt+0), 0x0, 0x300000016UL
};

EI_(base_GHCziErr_error_info);
II_(rVi_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag13_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Ah;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1Ah;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&rVi_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziErr_error_info);
_c1Ah:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag3_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag3_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag3_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager4_closure);
EI_(base_GHCziShow_zdwshowSignedInt_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Aq;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1Aq;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x0;
R3.w = 0x6UL;
R4.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager4_closure+2;
Sp=Sp-2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1Aq:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag3_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag2_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag3_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag2_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag2_info, 0x0, 0x0, 0x0
};

static char c1AA_str[] = ") is outside of enumeration\'s range (0,";

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag2_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag2_srt+0), 0x0, 0x100000016UL
};

EI_(base_GHCziBase_unpackAppendCStringzh_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag3_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1AD;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1AD;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1AA_str;
R3.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag3_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackAppendCStringzh_info);
_c1AD:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag4_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag4_info, 0x0, 0x0, 0x0
};

static char c1AM_str[] = "toEnum{Wochentag}: tag (";

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag4_info[] = {
0x0, 0x16UL
};

EI_(base_GHCziBase_unpackCStringzh_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1AP;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1AP;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = (W_)&c1AM_str;
Sp=Sp-2;
JMP_((W_)&base_GHCziBase_unpackCStringzh_info);
_c1AP:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziErr_error_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag2_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag4_closure);
StgWord Aufgabe3ziDatatypes_zdwtoEnum1_srt[] = {
(W_)&base_GHCziErr_error_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag2_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag4_closure
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum1_info);
StgWord Aufgabe3ziDatatypes_zdwtoEnum1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum1_info, 0x0
};

static StgWord s1AS_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_srt+8), 0x100000000UL, 0x100000012UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag2_closure);
EI_(base_GHCziShow_zdwshowSignedInt_info);
IF_(s1AS_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1B9;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = 0x0;
R3.w = R1.p[2];
R4.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag2_closure;
Sp=Sp-2;
JMP_((W_)&base_GHCziShow_zdwshowSignedInt_info);
_c1B9:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1AT_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_srt+0), 0x0, 0x100000022UL
};

EI_(base_GHCziErr_error_info);
IF_(s1AT_ret) {
FB_
R2.p=R1.p;
Sp=Sp+1;
JMP_((W_)&base_GHCziErr_error_info);
FE_
}

static StgWord s102_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_srt+0), 0x41UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag4_closure);
II_(s1AS_info);
II_(s102_info);
II_(s1AT_info);
IF_(s102_ret) {
FB_
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1Bd;
Hp[-2] = (W_)&s1AS_info;
*Hp = Sp[1];
R2.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag4_closure;
R3.p=Hp-2;
Sp[1] = (W_)&s1AT_info;
Sp=Sp+1;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c1Bd:
HpAlloc = 0x18UL;
*Sp = (W_)&s102_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
FE_
}

StgWord Aufgabe3ziDatatypes_zdwtoEnum1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_srt+0), 0x100000004UL, 0x0, 0x70000000fUL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum1_closure);
EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
II_(s102_info);
FN_(Aufgabe3ziDatatypes_zdwtoEnum1_entry) {
W_ _c1Bk;
W_ _c1Bl;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Bn;
Sp[-1] = R2.w;
_c1Bl = (W_)((I_)R2.w >= (I_)0x0);
if ((W_)(_c1Bl >= 0x1UL)) goto _c1Bp;
Sp=Sp-2;
JMP_((W_)&s102_info);
_c1Bn:
R1.w = (W_)&Aufgabe3ziDatatypes_zdwtoEnum1_closure;
JMP_(stg_gc_fun);
_c1Bp:
_c1Bk = (W_)((I_)R2.w <= (I_)0x6UL);
if ((W_)(_c1Bk >= 0x1UL)) goto _c1Bs;
Sp=Sp-2;
JMP_((W_)&s102_info);
_c1Bs:
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (R2.w << 0x3UL)));
JMP_(*Sp);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
StgWord Aufgabe3ziDatatypes_zdfShowLager4_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowLager4_info);
StgWord Aufgabe3ziDatatypes_zdfShowLager4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager4_info, 0x0
};

static StgWord s1Bv_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLager4_srt+0), 0x0, 0x700000022UL
};

EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s1Bv_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1BG;
case 0x1UL: goto _c1BI;
case 0x2UL: goto _c1BK;
}
_c1BG:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1BI:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1BK:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowLager4_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLager4_srt+0), 0x100000005UL, 0x0, 0x70000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowLager4_closure);
II_(s1Bv_info);
FN_(Aufgabe3ziDatatypes_zdfShowLager4_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1BN;
R1.p=R2.p;
Sp[-1] = (W_)&s1Bv_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1BQ;
JMP_(*R1.p);
_c1BN:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager4_closure;
JMP_(stg_gc_fun);
_c1BQ:
JMP_((W_)&s1Bv_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager1_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager1_info
};

static StgWord s1BV_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_LagerA_closure);
IF_(s1BV_ret) {
FB_
R1.w = (W_)&Aufgabe3ziDatatypes_LagerA_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1BU_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_LagerB_closure);
EI_(Aufgabe3ziDatatypes_LagerA_closure);
IF_(s1BU_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Cb;
case 0x1UL: goto _c1Cd;
case 0x2UL: goto _c1Cf;
}
_c1Cb:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerA_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Cd:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerB_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Cf:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerB_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1BT_info[] = {
0x1UL, 0x22UL
};

II_(s1BU_info);
II_(s1BV_info);
IF_(s1BT_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Ci;
case 0x1UL: goto _c1Ck;
case 0x2UL: goto _c1Cm;
}
_c1Ci:
R1.w = Sp[1];
Sp[1] = (W_)&s1BV_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Cr;
JMP_(*R1.p);
_c1Cr:
JMP_((W_)&s1BV_info);
_c1Ck:
R1.w = Sp[1];
Sp[1] = (W_)&s1BU_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Cu;
JMP_(*R1.p);
_c1Cu:
JMP_((W_)&s1BU_info);
_c1Cm:
R1.w = Sp[1];
Sp=Sp+2;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager1_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager1_closure);
II_(s1BT_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager1_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Cx;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1BT_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1CA;
JMP_(*R1.p);
_c1Cx:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager1_closure;
JMP_(stg_gc_fun);
_c1CA:
JMP_((W_)&s1BT_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager2_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager2_info
};

static StgWord s1CF_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_LagerC_closure);
EI_(Aufgabe3ziDatatypes_LagerB_closure);
IF_(s1CF_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1CU;
R1.w = (W_)&Aufgabe3ziDatatypes_LagerC_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1CU:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerB_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1CE_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_LagerC_closure);
IF_(s1CE_ret) {
FB_
R1.w = (W_)&Aufgabe3ziDatatypes_LagerC_closure+3;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1CD_info[] = {
0x1UL, 0x22UL
};

II_(s1CE_info);
II_(s1CF_info);
IF_(s1CD_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1CY;
case 0x1UL: goto _c1D0;
case 0x2UL: goto _c1D2;
}
_c1CY:
R1.w = Sp[1];
Sp=Sp+2;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1D0:
R1.w = Sp[1];
Sp[1] = (W_)&s1CF_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1D5;
JMP_(*R1.p);
_c1D5:
JMP_((W_)&s1CF_info);
_c1D2:
R1.w = Sp[1];
Sp[1] = (W_)&s1CE_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Db;
JMP_(*R1.p);
_c1Db:
JMP_((W_)&s1CE_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager2_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager2_closure);
II_(s1CD_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1De;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1CD_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Dh;
JMP_(*R1.p);
_c1De:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager2_closure;
JMP_(stg_gc_fun);
_c1Dh:
JMP_((W_)&s1CD_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager3_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager3_info
};

static StgWord s1Dn_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Dn_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Dm_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Dm_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1DD;
case 0x1UL: goto _c1DF;
case 0x2UL: goto _c1DH;
}
_c1DD:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1DF:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1DH:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Dl_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Dl_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1DO;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1DO:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Dk_info[] = {
0x1UL, 0x22UL
};

II_(s1Dl_info);
II_(s1Dm_info);
II_(s1Dn_info);
IF_(s1Dk_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1DR;
case 0x1UL: goto _c1DT;
case 0x2UL: goto _c1DV;
}
_c1DR:
R1.w = Sp[1];
Sp[1] = (W_)&s1Dn_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1E0;
JMP_(*R1.p);
_c1E0:
JMP_((W_)&s1Dn_info);
_c1DT:
R1.w = Sp[1];
Sp[1] = (W_)&s1Dm_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1E3;
JMP_(*R1.p);
_c1E3:
JMP_((W_)&s1Dm_info);
_c1DV:
R1.w = Sp[1];
Sp[1] = (W_)&s1Dl_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1E6;
JMP_(*R1.p);
_c1E6:
JMP_((W_)&s1Dl_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager3_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager3_closure);
II_(s1Dk_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1E9;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Dk_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Ec;
JMP_(*R1.p);
_c1E9:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager3_closure;
JMP_(stg_gc_fun);
_c1Ec:
JMP_((W_)&s1Dk_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager4_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager4_info
};

static StgWord s1Ei_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
IF_(s1Ei_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Eh_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Eh_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Ey;
case 0x1UL: goto _c1EA;
case 0x2UL: goto _c1EC;
}
_c1Ey:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1EA:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1EC:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Eg_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Eg_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1EJ;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1EJ:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Ef_info[] = {
0x1UL, 0x22UL
};

II_(s1Eg_info);
II_(s1Eh_info);
II_(s1Ei_info);
IF_(s1Ef_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1EM;
case 0x1UL: goto _c1EO;
case 0x2UL: goto _c1EQ;
}
_c1EM:
R1.w = Sp[1];
Sp[1] = (W_)&s1Ei_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1EV;
JMP_(*R1.p);
_c1EV:
JMP_((W_)&s1Ei_info);
_c1EO:
R1.w = Sp[1];
Sp[1] = (W_)&s1Eh_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1EY;
JMP_(*R1.p);
_c1EY:
JMP_((W_)&s1Eh_info);
_c1EQ:
R1.w = Sp[1];
Sp[1] = (W_)&s1Eg_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1F1;
JMP_(*R1.p);
_c1F1:
JMP_((W_)&s1Eg_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager4_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager4_closure);
II_(s1Ef_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1F4;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Ef_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1F7;
JMP_(*R1.p);
_c1F4:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager4_closure;
JMP_(stg_gc_fun);
_c1F7:
JMP_((W_)&s1Ef_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager5_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager5_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager5_info
};

static StgWord s1Fd_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Fd_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Fr;
case 0x1UL: goto _c1Ft;
case 0x2UL: goto _c1Fv;
}
_c1Fr:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ft:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Fv:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Fc_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Fc_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1FC;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1FC:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Fb_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Fb_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Fa_info[] = {
0x1UL, 0x22UL
};

II_(s1Fb_info);
II_(s1Fc_info);
II_(s1Fd_info);
IF_(s1Fa_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1FG;
case 0x1UL: goto _c1FI;
case 0x2UL: goto _c1FK;
}
_c1FG:
R1.w = Sp[1];
Sp[1] = (W_)&s1Fd_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1FN;
JMP_(*R1.p);
_c1FN:
JMP_((W_)&s1Fd_info);
_c1FI:
R1.w = Sp[1];
Sp[1] = (W_)&s1Fc_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1FQ;
JMP_(*R1.p);
_c1FQ:
JMP_((W_)&s1Fc_info);
_c1FK:
R1.w = Sp[1];
Sp[1] = (W_)&s1Fb_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1FW;
JMP_(*R1.p);
_c1FW:
JMP_((W_)&s1Fb_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager5_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager5_closure);
II_(s1Fa_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager5_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1FZ;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Fa_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1G2;
JMP_(*R1.p);
_c1FZ:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager5_closure;
JMP_(stg_gc_fun);
_c1G2:
JMP_((W_)&s1Fa_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdLager6_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLager6_info
};

static StgWord s1G8_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1G8_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Gm;
case 0x1UL: goto _c1Go;
case 0x2UL: goto _c1Gq;
}
_c1Gm:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Go:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Gq:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1G7_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1G7_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c1Gx;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Gx:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1G6_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
IF_(s1G6_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1G5_info[] = {
0x1UL, 0x22UL
};

II_(s1G6_info);
II_(s1G7_info);
II_(s1G8_info);
IF_(s1G5_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1GB;
case 0x1UL: goto _c1GD;
case 0x2UL: goto _c1GF;
}
_c1GB:
R1.w = Sp[1];
Sp[1] = (W_)&s1G8_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1GI;
JMP_(*R1.p);
_c1GI:
JMP_((W_)&s1G8_info);
_c1GD:
R1.w = Sp[1];
Sp[1] = (W_)&s1G7_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1GL;
JMP_(*R1.p);
_c1GL:
JMP_((W_)&s1G7_info);
_c1GF:
R1.w = Sp[1];
Sp[1] = (W_)&s1G6_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1GR;
JMP_(*R1.p);
_c1GR:
JMP_((W_)&s1G6_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLager6_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLager6_closure);
II_(s1G5_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLager6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1GU;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1G5_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1GX;
JMP_(*R1.p);
_c1GU:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLager6_closure;
JMP_(stg_gc_fun);
_c1GX:
JMP_((W_)&s1G5_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag1_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag1_info
};

static StgWord s1H2_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Montag_closure);
IF_(s1H2_ret) {
FB_
R1.w = (W_)&Aufgabe3ziDatatypes_Montag_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1H1_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Dienstag_closure);
EI_(Aufgabe3ziDatatypes_Montag_closure);
IF_(s1H1_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Hi;
case 0x1UL: goto _c1Hk;
case 0x2UL: goto _c1Hm;
case 0x3UL: goto _c1Ho;
case 0x4UL: goto _c1Hq;
case 0x5UL: goto _c1Hs;
case 0x6UL: goto _c1Hu;
}
_c1Hi:
R1.w = (W_)&Aufgabe3ziDatatypes_Montag_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Hk:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Hm:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ho:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Hq:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Hs:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Hu:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s110_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
IF_(s110_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1HB;
case 0x2UL: goto _c1HD;
case 0x3UL: goto _c1HF;
case 0x4UL: goto _c1HH;
case 0x5UL: goto _c1HJ;
case 0x6UL: goto _c1HL;
}
_c1HB:
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1HD:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1HF:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1HH:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1HJ:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1HL:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s112_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
IF_(s112_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1HS;
case 0x3UL: goto _c1HU;
case 0x4UL: goto _c1HW;
case 0x5UL: goto _c1HY;
case 0x6UL: goto _c1I0;
}
_c1HS:
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1HU:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1HW:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1HY:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1I0:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s114_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Freitag_closure);
IF_(s114_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1I7;
case 0x4UL: goto _c1I9;
case 0x5UL: goto _c1Ib;
case 0x6UL: goto _c1Id;
}
_c1I7:
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1I9:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1Ib:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1Id:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s116_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Samstag_closure);
IF_(s116_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1Ik;
case 0x5UL: goto _c1Im;
case 0x6UL: goto _c1Io;
}
_c1Ik:
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1Im:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c1Io:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1H0_info[] = {
0x1UL, 0x22UL
};

II_(s110_info);
II_(s112_info);
II_(s114_info);
II_(s116_info);
II_(s1H1_info);
II_(s1H2_info);
IF_(s1H0_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Ir;
case 0x1UL: goto _c1It;
case 0x2UL: goto _c1Iv;
case 0x3UL: goto _c1Ix;
case 0x4UL: goto _c1Iz;
case 0x5UL: goto _c1IB;
case 0x6UL: goto _c1ID;
}
_c1Ir:
R1.w = Sp[1];
Sp[1] = (W_)&s1H2_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1II;
JMP_(*R1.p);
_c1II:
JMP_((W_)&s1H2_info);
_c1It:
R1.w = Sp[1];
Sp[1] = (W_)&s1H1_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1IL;
JMP_(*R1.p);
_c1IL:
JMP_((W_)&s1H1_info);
_c1Iv:
R1.w = Sp[1];
Sp[1] = (W_)&s110_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1IO;
JMP_(*R1.p);
_c1IO:
JMP_((W_)&s110_info);
_c1Ix:
R1.w = Sp[1];
Sp[1] = (W_)&s112_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1IR;
JMP_(*R1.p);
_c1IR:
JMP_((W_)&s112_info);
_c1Iz:
R1.w = Sp[1];
Sp[1] = (W_)&s114_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1IU;
JMP_(*R1.p);
_c1IU:
JMP_((W_)&s114_info);
_c1IB:
R1.w = Sp[1];
Sp[1] = (W_)&s116_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1IX;
JMP_(*R1.p);
_c1IX:
JMP_((W_)&s116_info);
_c1ID:
R1.w = Sp[1];
Sp=Sp+2;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag1_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag1_closure);
II_(s1H0_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag1_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1J0;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1H0_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1J3;
JMP_(*R1.p);
_c1J0:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag1_closure;
JMP_(stg_gc_fun);
_c1J3:
JMP_((W_)&s1H0_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag2_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag2_info
};

static StgWord s1Jc_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
EI_(Aufgabe3ziDatatypes_Dienstag_closure);
IF_(s1Jc_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1Jr;
case 0x2UL: goto _c1Jt;
case 0x3UL: goto _c1Jv;
case 0x4UL: goto _c1Jx;
case 0x5UL: goto _c1Jz;
case 0x6UL: goto _c1JB;
}
_c1Jr:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Jt:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1Jv:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1Jx:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1Jz:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c1JB:
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Jb_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
IF_(s1Jb_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1JI;
case 0x3UL: goto _c1JK;
case 0x4UL: goto _c1JM;
case 0x5UL: goto _c1JO;
case 0x6UL: goto _c1JQ;
}
_c1JI:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1JK:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1JM:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1JO:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c1JQ:
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Ja_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
IF_(s1Ja_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1JX;
case 0x4UL: goto _c1JZ;
case 0x5UL: goto _c1K1;
case 0x6UL: goto _c1K3;
}
_c1JX:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c1JZ:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1K1:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c1K3:
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1J9_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
IF_(s1J9_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1Ka;
case 0x5UL: goto _c1Kc;
case 0x6UL: goto _c1Ke;
}
_c1Ka:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c1Kc:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c1Ke:
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1J8_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
IF_(s1J8_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1Kl;
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
_c1Kl:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1J7_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_Sonntag_closure);
IF_(s1J7_ret) {
FB_
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1J6_info[] = {
0x1UL, 0x22UL
};

II_(s1J7_info);
II_(s1J8_info);
II_(s1J9_info);
II_(s1Ja_info);
II_(s1Jb_info);
II_(s1Jc_info);
IF_(s1J6_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Kp;
case 0x1UL: goto _c1Kr;
case 0x2UL: goto _c1Kt;
case 0x3UL: goto _c1Kv;
case 0x4UL: goto _c1Kx;
case 0x5UL: goto _c1Kz;
case 0x6UL: goto _c1KB;
}
_c1Kp:
R1.w = Sp[1];
Sp=Sp+2;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c1Kr:
R1.w = Sp[1];
Sp[1] = (W_)&s1Jc_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KE;
JMP_(*R1.p);
_c1KE:
JMP_((W_)&s1Jc_info);
_c1Kt:
R1.w = Sp[1];
Sp[1] = (W_)&s1Jb_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KH;
JMP_(*R1.p);
_c1KH:
JMP_((W_)&s1Jb_info);
_c1Kv:
R1.w = Sp[1];
Sp[1] = (W_)&s1Ja_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KK;
JMP_(*R1.p);
_c1KK:
JMP_((W_)&s1Ja_info);
_c1Kx:
R1.w = Sp[1];
Sp[1] = (W_)&s1J9_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KN;
JMP_(*R1.p);
_c1KN:
JMP_((W_)&s1J9_info);
_c1Kz:
R1.w = Sp[1];
Sp[1] = (W_)&s1J8_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KQ;
JMP_(*R1.p);
_c1KQ:
JMP_((W_)&s1J8_info);
_c1KB:
R1.w = Sp[1];
Sp[1] = (W_)&s1J7_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1KW;
JMP_(*R1.p);
_c1KW:
JMP_((W_)&s1J7_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag2_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag2_closure);
II_(s1J6_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag2_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1KZ;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1J6_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1L2;
JMP_(*R1.p);
_c1KZ:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag2_closure;
JMP_(stg_gc_fun);
_c1L2:
JMP_((W_)&s1J6_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag3_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag3_info
};

static StgWord s1Lc_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Lc_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Lb_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Lb_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Ls;
case 0x1UL: goto _c1Lu;
case 0x2UL: goto _c1Lw;
case 0x3UL: goto _c1Ly;
case 0x4UL: goto _c1LA;
case 0x5UL: goto _c1LC;
case 0x6UL: goto _c1LE;
}
_c1Ls:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Lu:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Lw:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ly:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LA:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LC:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LE:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1La_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1La_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1LL;
case 0x2UL: goto _c1LN;
case 0x3UL: goto _c1LP;
case 0x4UL: goto _c1LR;
case 0x5UL: goto _c1LT;
case 0x6UL: goto _c1LV;
}
_c1LL:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1LN:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LP:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LR:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LT:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1LV:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1L9_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1L9_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1M2;
case 0x3UL: goto _c1M4;
case 0x4UL: goto _c1M6;
case 0x5UL: goto _c1M8;
case 0x6UL: goto _c1Ma;
}
_c1M2:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1M4:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1M6:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1M8:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ma:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1L8_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1L8_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1Mh;
case 0x4UL: goto _c1Mj;
case 0x5UL: goto _c1Ml;
case 0x6UL: goto _c1Mn;
}
_c1Mh:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Mj:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ml:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Mn:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1L7_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1L7_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1Mu;
case 0x5UL: goto _c1Mw;
case 0x6UL: goto _c1My;
}
_c1Mu:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Mw:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1My:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1L6_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1L6_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1MF;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1MF:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1L5_info[] = {
0x1UL, 0x22UL
};

II_(s1L6_info);
II_(s1L7_info);
II_(s1L8_info);
II_(s1L9_info);
II_(s1La_info);
II_(s1Lb_info);
II_(s1Lc_info);
IF_(s1L5_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1MI;
case 0x1UL: goto _c1MK;
case 0x2UL: goto _c1MM;
case 0x3UL: goto _c1MO;
case 0x4UL: goto _c1MQ;
case 0x5UL: goto _c1MS;
case 0x6UL: goto _c1MU;
}
_c1MI:
R1.w = Sp[1];
Sp[1] = (W_)&s1Lc_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1MZ;
JMP_(*R1.p);
_c1MZ:
JMP_((W_)&s1Lc_info);
_c1MK:
R1.w = Sp[1];
Sp[1] = (W_)&s1Lb_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1N2;
JMP_(*R1.p);
_c1N2:
JMP_((W_)&s1Lb_info);
_c1MM:
R1.w = Sp[1];
Sp[1] = (W_)&s1La_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1N5;
JMP_(*R1.p);
_c1N5:
JMP_((W_)&s1La_info);
_c1MO:
R1.w = Sp[1];
Sp[1] = (W_)&s1L9_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1N8;
JMP_(*R1.p);
_c1N8:
JMP_((W_)&s1L9_info);
_c1MQ:
R1.w = Sp[1];
Sp[1] = (W_)&s1L8_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Nb;
JMP_(*R1.p);
_c1Nb:
JMP_((W_)&s1L8_info);
_c1MS:
R1.w = Sp[1];
Sp[1] = (W_)&s1L7_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Ne;
JMP_(*R1.p);
_c1Ne:
JMP_((W_)&s1L7_info);
_c1MU:
R1.w = Sp[1];
Sp[1] = (W_)&s1L6_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Nh;
JMP_(*R1.p);
_c1Nh:
JMP_((W_)&s1L6_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag3_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag3_closure);
II_(s1L5_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag3_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Nk;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1L5_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Nn;
JMP_(*R1.p);
_c1Nk:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag3_closure;
JMP_(stg_gc_fun);
_c1Nn:
JMP_((W_)&s1L5_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag4_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag4_info
};

static StgWord s1Nx_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
IF_(s1Nx_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nw_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Nw_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1NN;
case 0x1UL: goto _c1NP;
case 0x2UL: goto _c1NR;
case 0x3UL: goto _c1NT;
case 0x4UL: goto _c1NV;
case 0x5UL: goto _c1NX;
case 0x6UL: goto _c1NZ;
}
_c1NN:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1NP:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1NR:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1NT:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1NV:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1NX:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1NZ:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nv_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Nv_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1O6;
case 0x2UL: goto _c1O8;
case 0x3UL: goto _c1Oa;
case 0x4UL: goto _c1Oc;
case 0x5UL: goto _c1Oe;
case 0x6UL: goto _c1Og;
}
_c1O6:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1O8:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Oa:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Oc:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Oe:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Og:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nu_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Nu_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1On;
case 0x3UL: goto _c1Op;
case 0x4UL: goto _c1Or;
case 0x5UL: goto _c1Ot;
case 0x6UL: goto _c1Ov;
}
_c1On:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Op:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Or:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Ot:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Ov:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nt_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Nt_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1OC;
case 0x4UL: goto _c1OE;
case 0x5UL: goto _c1OG;
case 0x6UL: goto _c1OI;
}
_c1OC:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1OE:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1OG:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1OI:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Ns_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Ns_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1OP;
case 0x5UL: goto _c1OR;
case 0x6UL: goto _c1OT;
}
_c1OP:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1OR:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1OT:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nr_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Nr_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1P0;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1P0:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Nq_info[] = {
0x1UL, 0x22UL
};

II_(s1Nr_info);
II_(s1Ns_info);
II_(s1Nt_info);
II_(s1Nu_info);
II_(s1Nv_info);
II_(s1Nw_info);
II_(s1Nx_info);
IF_(s1Nq_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1P3;
case 0x1UL: goto _c1P5;
case 0x2UL: goto _c1P7;
case 0x3UL: goto _c1P9;
case 0x4UL: goto _c1Pb;
case 0x5UL: goto _c1Pd;
case 0x6UL: goto _c1Pf;
}
_c1P3:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nx_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pk;
JMP_(*R1.p);
_c1Pk:
JMP_((W_)&s1Nx_info);
_c1P5:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nw_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pn;
JMP_(*R1.p);
_c1Pn:
JMP_((W_)&s1Nw_info);
_c1P7:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nv_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pq;
JMP_(*R1.p);
_c1Pq:
JMP_((W_)&s1Nv_info);
_c1P9:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nu_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pt;
JMP_(*R1.p);
_c1Pt:
JMP_((W_)&s1Nu_info);
_c1Pb:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nt_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pw;
JMP_(*R1.p);
_c1Pw:
JMP_((W_)&s1Nt_info);
_c1Pd:
R1.w = Sp[1];
Sp[1] = (W_)&s1Ns_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Pz;
JMP_(*R1.p);
_c1Pz:
JMP_((W_)&s1Ns_info);
_c1Pf:
R1.w = Sp[1];
Sp[1] = (W_)&s1Nr_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1PC;
JMP_(*R1.p);
_c1PC:
JMP_((W_)&s1Nr_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag4_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag4_closure);
II_(s1Nq_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1PF;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Nq_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1PI;
JMP_(*R1.p);
_c1PF:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag4_closure;
JMP_(stg_gc_fun);
_c1PI:
JMP_((W_)&s1Nq_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag5_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag5_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag5_info
};

static StgWord s1PS_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PS_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Q6;
case 0x1UL: goto _c1Q8;
case 0x2UL: goto _c1Qa;
case 0x3UL: goto _c1Qc;
case 0x4UL: goto _c1Qe;
case 0x5UL: goto _c1Qg;
case 0x6UL: goto _c1Qi;
}
_c1Q6:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Q8:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qa:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qc:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qe:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qg:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qi:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PR_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PR_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1Qp;
case 0x2UL: goto _c1Qr;
case 0x3UL: goto _c1Qt;
case 0x4UL: goto _c1Qv;
case 0x5UL: goto _c1Qx;
case 0x6UL: goto _c1Qz;
}
_c1Qp:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Qr:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qt:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qv:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qx:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Qz:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PQ_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PQ_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1QG;
case 0x3UL: goto _c1QI;
case 0x4UL: goto _c1QK;
case 0x5UL: goto _c1QM;
case 0x6UL: goto _c1QO;
}
_c1QG:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1QI:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1QK:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1QM:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1QO:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PP_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PP_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1QV;
case 0x4UL: goto _c1QX;
case 0x5UL: goto _c1QZ;
case 0x6UL: goto _c1R1;
}
_c1QV:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1QX:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1QZ:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1R1:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PO_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PO_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1R8;
case 0x5UL: goto _c1Ra;
case 0x6UL: goto _c1Rc;
}
_c1R8:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Ra:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Rc:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PN_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PN_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1Rj;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Rj:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PM_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1PM_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1PL_info[] = {
0x1UL, 0x22UL
};

II_(s1PM_info);
II_(s1PN_info);
II_(s1PO_info);
II_(s1PP_info);
II_(s1PQ_info);
II_(s1PR_info);
II_(s1PS_info);
IF_(s1PL_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Rn;
case 0x1UL: goto _c1Rp;
case 0x2UL: goto _c1Rr;
case 0x3UL: goto _c1Rt;
case 0x4UL: goto _c1Rv;
case 0x5UL: goto _c1Rx;
case 0x6UL: goto _c1Rz;
}
_c1Rn:
R1.w = Sp[1];
Sp[1] = (W_)&s1PS_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RC;
JMP_(*R1.p);
_c1RC:
JMP_((W_)&s1PS_info);
_c1Rp:
R1.w = Sp[1];
Sp[1] = (W_)&s1PR_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RF;
JMP_(*R1.p);
_c1RF:
JMP_((W_)&s1PR_info);
_c1Rr:
R1.w = Sp[1];
Sp[1] = (W_)&s1PQ_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RI;
JMP_(*R1.p);
_c1RI:
JMP_((W_)&s1PQ_info);
_c1Rt:
R1.w = Sp[1];
Sp[1] = (W_)&s1PP_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RL;
JMP_(*R1.p);
_c1RL:
JMP_((W_)&s1PP_info);
_c1Rv:
R1.w = Sp[1];
Sp[1] = (W_)&s1PO_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RO;
JMP_(*R1.p);
_c1RO:
JMP_((W_)&s1PO_info);
_c1Rx:
R1.w = Sp[1];
Sp[1] = (W_)&s1PN_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RR;
JMP_(*R1.p);
_c1RR:
JMP_((W_)&s1PN_info);
_c1Rz:
R1.w = Sp[1];
Sp[1] = (W_)&s1PM_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1RX;
JMP_(*R1.p);
_c1RX:
JMP_((W_)&s1PM_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag5_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag5_closure);
II_(s1PL_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag5_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1S0;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1PL_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1S3;
JMP_(*R1.p);
_c1S0:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag5_closure;
JMP_(stg_gc_fun);
_c1S3:
JMP_((W_)&s1PL_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag6_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentag6_info
};

static StgWord s1Sd_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Sd_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Sr;
case 0x1UL: goto _c1St;
case 0x2UL: goto _c1Sv;
case 0x3UL: goto _c1Sx;
case 0x4UL: goto _c1Sz;
case 0x5UL: goto _c1SB;
case 0x6UL: goto _c1SD;
}
_c1Sr:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1St:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Sv:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Sx:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Sz:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SB:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SD:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Sc_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Sc_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1SK;
case 0x2UL: goto _c1SM;
case 0x3UL: goto _c1SO;
case 0x4UL: goto _c1SQ;
case 0x5UL: goto _c1SS;
case 0x6UL: goto _c1SU;
}
_c1SK:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1SM:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SO:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SQ:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SS:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1SU:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Sb_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Sb_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1T1;
case 0x3UL: goto _c1T3;
case 0x4UL: goto _c1T5;
case 0x5UL: goto _c1T7;
case 0x6UL: goto _c1T9;
}
_c1T1:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1T3:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1T5:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1T7:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1T9:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Sa_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1Sa_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1Tg;
case 0x4UL: goto _c1Ti;
case 0x5UL: goto _c1Tk;
case 0x6UL: goto _c1Tm;
}
_c1Tg:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Ti:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Tk:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Tm:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1S9_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1S9_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1Tt;
case 0x5UL: goto _c1Tv;
case 0x6UL: goto _c1Tx;
}
_c1Tt:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Tv:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Tx:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1S8_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s1S8_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1TE;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1TE:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1S7_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
IF_(s1S7_ret) {
FB_
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1S6_info[] = {
0x1UL, 0x22UL
};

II_(s1S7_info);
II_(s1S8_info);
II_(s1S9_info);
II_(s1Sa_info);
II_(s1Sb_info);
II_(s1Sc_info);
II_(s1Sd_info);
IF_(s1S6_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1TI;
case 0x1UL: goto _c1TK;
case 0x2UL: goto _c1TM;
case 0x3UL: goto _c1TO;
case 0x4UL: goto _c1TQ;
case 0x5UL: goto _c1TS;
case 0x6UL: goto _c1TU;
}
_c1TI:
R1.w = Sp[1];
Sp[1] = (W_)&s1Sd_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1TX;
JMP_(*R1.p);
_c1TX:
JMP_((W_)&s1Sd_info);
_c1TK:
R1.w = Sp[1];
Sp[1] = (W_)&s1Sc_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1U0;
JMP_(*R1.p);
_c1U0:
JMP_((W_)&s1Sc_info);
_c1TM:
R1.w = Sp[1];
Sp[1] = (W_)&s1Sb_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1U3;
JMP_(*R1.p);
_c1U3:
JMP_((W_)&s1Sb_info);
_c1TO:
R1.w = Sp[1];
Sp[1] = (W_)&s1Sa_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1U6;
JMP_(*R1.p);
_c1U6:
JMP_((W_)&s1Sa_info);
_c1TQ:
R1.w = Sp[1];
Sp[1] = (W_)&s1S9_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1U9;
JMP_(*R1.p);
_c1U9:
JMP_((W_)&s1S9_info);
_c1TS:
R1.w = Sp[1];
Sp[1] = (W_)&s1S8_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Uc;
JMP_(*R1.p);
_c1Uc:
JMP_((W_)&s1S8_info);
_c1TU:
R1.w = Sp[1];
Sp[1] = (W_)&s1S7_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Ui;
JMP_(*R1.p);
_c1Ui:
JMP_((W_)&s1S7_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentag6_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentag6_closure);
II_(s1S6_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentag6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c1Ul;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1S6_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Uo;
JMP_(*R1.p);
_c1Ul:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentag6_closure;
JMP_(stg_gc_fun);
_c1Uo:
JMP_((W_)&s1S6_info);
FE_
}
EI_(Aufgabe3ziDatatypes_tourAnwenden_info);
StgWord Aufgabe3ziDatatypes_tourAnwenden_closure[] = {
(W_)&Aufgabe3ziDatatypes_tourAnwenden_info
};

static StgWord s1Ut_info[] = {
0x0, 0x22UL
};

EI_(Aufgabe3ziDatatypes_TourSet_con_info);
IF_(s1Ut_ret) {
FB_
Hp=Hp+7;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c1UM;
Hp[-6] = (W_)&Aufgabe3ziDatatypes_TourSet_con_info;
Hp[-5] = R1.w;
Hp[-4] = R2.w;
Hp[-3] = R3.w;
Hp[-2] = R4.w;
Hp[-1] = R5.w;
*Hp = R6.w;
R1.w = (W_)Hp-47;
Sp=Sp+1;
JMP_(*Sp);
_c1UM:
HpAlloc = 0x38UL;
R9.w = 0xc0UL;
JMP_((W_)&stg_gc_ut);
FE_
}

static StgWord s1Us_info[] = {
0x3UL, 0x22UL
};

EI_(Aufgabe3ziDatatypes_zdwtourAnwenden_info);
II_(s1Ut_info);
IF_(s1Us_ret) {
FB_
*Sp = *((P_)(R1.w+31));
Sp[-1] = *((P_)(R1.w+23));
R2.w = Sp[3];
R3.w = Sp[2];
Sp[2] = *((P_)(R1.w+47));
R4.w = Sp[1];
Sp[1] = *((P_)(R1.w+39));
R5.w = *((P_)(R1.w+7));
R6.w = *((P_)(R1.w+15));
Sp[3] = (W_)&s1Ut_info;
Sp=Sp-1;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtourAnwenden_info);
FE_
}

static StgWord s1Ur_info[] = {
0x1UL, 0x22UL
};

II_(s1Us_info);
IF_(s1Ur_ret) {
W_ _c1UR;
FB_
Sp[-1] = *((P_)(R1.w+23));
*Sp = *((P_)(R1.w+15));
_c1UR = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c1UR;
Sp[-2] = (W_)&s1Us_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1UT;
JMP_(*R1.p);
_c1UT:
JMP_((W_)&s1Us_info);
FE_
}

StgWord Aufgabe3ziDatatypes_tourAnwenden_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_tourAnwenden_closure);
II_(s1Ur_info);
FN_(Aufgabe3ziDatatypes_tourAnwenden_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c1UW;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Ur_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1UZ;
JMP_(*R1.p);
_c1UW:
R1.w = (W_)&Aufgabe3ziDatatypes_tourAnwenden_closure;
JMP_(stg_gc_fun);
_c1UZ:
JMP_((W_)&s1Ur_info);
FE_
}
EI_(Aufgabe3ziDatatypes_fahrten_info);
StgWord Aufgabe3ziDatatypes_fahrten_closure[] = {
(W_)&Aufgabe3ziDatatypes_fahrten_info
};

static StgWord s1V2_info[] = {
0x0, 0x22UL
};

IF_(s1V2_ret) {
FB_
R1.w = *((P_)(R1.w+23));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_fahrten_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_fahrten_closure);
II_(s1V2_info);
FN_(Aufgabe3ziDatatypes_fahrten_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1Vd;
R1.p=R2.p;
Sp[-1] = (W_)&s1V2_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Vh;
JMP_(*R1.p);
_c1Vd:
R1.w = (W_)&Aufgabe3ziDatatypes_fahrten_closure;
JMP_(stg_gc_fun);
_c1Vh:
JMP_((W_)&s1V2_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zziel_info);
StgWord Aufgabe3ziDatatypes_zziel_closure[] = {
(W_)&Aufgabe3ziDatatypes_zziel_info
};

static StgWord s1Vk_info[] = {
0x0, 0x22UL
};

IF_(s1Vk_ret) {
FB_
R1.w = *((P_)(R1.w+15));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zziel_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zziel_closure);
II_(s1Vk_info);
FN_(Aufgabe3ziDatatypes_zziel_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1Vv;
R1.p=R2.p;
Sp[-1] = (W_)&s1Vk_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Vz;
JMP_(*R1.p);
_c1Vv:
R1.w = (W_)&Aufgabe3ziDatatypes_zziel_closure;
JMP_(stg_gc_fun);
_c1Vz:
JMP_((W_)&s1Vk_info);
FE_
}
EI_(Aufgabe3ziDatatypes_start_info);
StgWord Aufgabe3ziDatatypes_start_closure[] = {
(W_)&Aufgabe3ziDatatypes_start_info
};

static StgWord s1VC_info[] = {
0x0, 0x22UL
};

IF_(s1VC_ret) {
FB_
R1.w = *((P_)(R1.w+7));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_start_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_start_closure);
II_(s1VC_info);
FN_(Aufgabe3ziDatatypes_start_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1VN;
R1.p=R2.p;
Sp[-1] = (W_)&s1VC_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1VR;
JMP_(*R1.p);
_c1VN:
R1.w = (W_)&Aufgabe3ziDatatypes_start_closure;
JMP_(stg_gc_fun);
_c1VR:
JMP_((W_)&s1VC_info);
FE_
}
EI_(Aufgabe3ziDatatypes_cZZuB_info);
StgWord Aufgabe3ziDatatypes_cZZuB_closure[] = {
(W_)&Aufgabe3ziDatatypes_cZZuB_info
};

static StgWord s1VU_info[] = {
0x0, 0x22UL
};

IF_(s1VU_ret) {
FB_
R1.w = *((P_)(R1.w+47));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_cZZuB_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_cZZuB_closure);
II_(s1VU_info);
FN_(Aufgabe3ziDatatypes_cZZuB_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1W8;
R1.p=R2.p;
Sp[-1] = (W_)&s1VU_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Wc;
JMP_(*R1.p);
_c1W8:
R1.w = (W_)&Aufgabe3ziDatatypes_cZZuB_closure;
JMP_(stg_gc_fun);
_c1Wc:
JMP_((W_)&s1VU_info);
FE_
}
EI_(Aufgabe3ziDatatypes_cZZuA_info);
StgWord Aufgabe3ziDatatypes_cZZuA_closure[] = {
(W_)&Aufgabe3ziDatatypes_cZZuA_info
};

static StgWord s1Wf_info[] = {
0x0, 0x22UL
};

IF_(s1Wf_ret) {
FB_
R1.w = *((P_)(R1.w+39));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_cZZuA_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_cZZuA_closure);
II_(s1Wf_info);
FN_(Aufgabe3ziDatatypes_cZZuA_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1Wt;
R1.p=R2.p;
Sp[-1] = (W_)&s1Wf_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Wx;
JMP_(*R1.p);
_c1Wt:
R1.w = (W_)&Aufgabe3ziDatatypes_cZZuA_closure;
JMP_(stg_gc_fun);
_c1Wx:
JMP_((W_)&s1Wf_info);
FE_
}
EI_(Aufgabe3ziDatatypes_bZZuC_info);
StgWord Aufgabe3ziDatatypes_bZZuC_closure[] = {
(W_)&Aufgabe3ziDatatypes_bZZuC_info
};

static StgWord s1WA_info[] = {
0x0, 0x22UL
};

IF_(s1WA_ret) {
FB_
R1.w = *((P_)(R1.w+31));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_bZZuC_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_bZZuC_closure);
II_(s1WA_info);
FN_(Aufgabe3ziDatatypes_bZZuC_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1WO;
R1.p=R2.p;
Sp[-1] = (W_)&s1WA_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1WS;
JMP_(*R1.p);
_c1WO:
R1.w = (W_)&Aufgabe3ziDatatypes_bZZuC_closure;
JMP_(stg_gc_fun);
_c1WS:
JMP_((W_)&s1WA_info);
FE_
}
EI_(Aufgabe3ziDatatypes_bZZuA_info);
StgWord Aufgabe3ziDatatypes_bZZuA_closure[] = {
(W_)&Aufgabe3ziDatatypes_bZZuA_info
};

static StgWord s1WV_info[] = {
0x0, 0x22UL
};

IF_(s1WV_ret) {
FB_
R1.w = *((P_)(R1.w+23));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_bZZuA_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_bZZuA_closure);
II_(s1WV_info);
FN_(Aufgabe3ziDatatypes_bZZuA_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1X9;
R1.p=R2.p;
Sp[-1] = (W_)&s1WV_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Xd;
JMP_(*R1.p);
_c1X9:
R1.w = (W_)&Aufgabe3ziDatatypes_bZZuA_closure;
JMP_(stg_gc_fun);
_c1Xd:
JMP_((W_)&s1WV_info);
FE_
}
EI_(Aufgabe3ziDatatypes_aZZuC_info);
StgWord Aufgabe3ziDatatypes_aZZuC_closure[] = {
(W_)&Aufgabe3ziDatatypes_aZZuC_info
};

static StgWord s1Xg_info[] = {
0x0, 0x22UL
};

IF_(s1Xg_ret) {
FB_
R1.w = *((P_)(R1.w+15));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_aZZuC_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_aZZuC_closure);
II_(s1Xg_info);
FN_(Aufgabe3ziDatatypes_aZZuC_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1Xu;
R1.p=R2.p;
Sp[-1] = (W_)&s1Xg_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1Xy;
JMP_(*R1.p);
_c1Xu:
R1.w = (W_)&Aufgabe3ziDatatypes_aZZuC_closure;
JMP_(stg_gc_fun);
_c1Xy:
JMP_((W_)&s1Xg_info);
FE_
}
EI_(Aufgabe3ziDatatypes_aZZuB_info);
StgWord Aufgabe3ziDatatypes_aZZuB_closure[] = {
(W_)&Aufgabe3ziDatatypes_aZZuB_info
};

static StgWord s1XB_info[] = {
0x0, 0x22UL
};

IF_(s1XB_ret) {
FB_
R1.w = *((P_)(R1.w+7));
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_aZZuB_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_aZZuB_closure);
II_(s1XB_info);
FN_(Aufgabe3ziDatatypes_aZZuB_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c1XP;
R1.p=R2.p;
Sp[-1] = (W_)&s1XB_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c1XT;
JMP_(*R1.p);
_c1XP:
R1.w = (W_)&Aufgabe3ziDatatypes_aZZuB_closure;
JMP_(stg_gc_fun);
_c1XT:
JMP_((W_)&s1XB_info);
FE_
}
EI_(Aufgabe3ziDatatypes_LagerC_closure);
EI_(Aufgabe3ziDatatypes_LagerA_closure);
EI_(base_GHCziEnum_DZCBounded_static_info);
StgWord Aufgabe3ziDatatypes_zdfBoundedLager_closure[] = {
(W_)&base_GHCziEnum_DZCBounded_static_info, ((W_)&Aufgabe3ziDatatypes_LagerA_closure+1), ((W_)&Aufgabe3ziDatatypes_LagerC_closure+3), 0x1UL
};
EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Montag_closure);
EI_(base_GHCziEnum_DZCBounded_static_info);
StgWord Aufgabe3ziDatatypes_zdfBoundedWochentag_closure[] = {
(W_)&base_GHCziEnum_DZCBounded_static_info, ((W_)&Aufgabe3ziDatatypes_Montag_closure+1), ((W_)&Aufgabe3ziDatatypes_Sonntag_closure+7), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_info
};

static StgWord s1Y9_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y9_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1Yp;
case 0x1UL: goto _c1Yr;
case 0x2UL: goto _c1Yt;
case 0x3UL: goto _c1Yv;
case 0x4UL: goto _c1Yx;
case 0x5UL: goto _c1Yz;
case 0x6UL: goto _c1YB;
}
_c1Yp:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Yr:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Yt:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Yv:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Yx:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Yz:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1YB:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y8_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y8_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1YI;
case 0x1UL: goto _c1YK;
case 0x2UL: goto _c1YM;
case 0x3UL: goto _c1YO;
case 0x4UL: goto _c1YQ;
case 0x5UL: goto _c1YS;
case 0x6UL: goto _c1YU;
}
_c1YI:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1YK:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1YM:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1YO:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1YQ:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1YS:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1YU:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y7_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y7_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x0: goto _c1Z1;
case 0x2UL: goto _c1Z3;
case 0x3UL: goto _c1Z5;
case 0x4UL: goto _c1Z7;
case 0x5UL: goto _c1Z9;
case 0x6UL: goto _c1Zb;
}
_c1Z1:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1Z3:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Z5:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Z7:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Z9:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Zb:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y6_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y6_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x0: goto _c1Zi;
case 0x3UL: goto _c1Zk;
case 0x4UL: goto _c1Zm;
case 0x5UL: goto _c1Zo;
case 0x6UL: goto _c1Zq;
}
_c1Zi:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1Zk:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1Zm:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Zo:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1Zq:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y5_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y5_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x0: goto _c1Zx;
case 0x4UL: goto _c1Zz;
case 0x5UL: goto _c1ZB;
case 0x6UL: goto _c1ZD;
}
_c1Zx:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1Zz:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1ZB:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c1ZD:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y4_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s1Y4_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x1UL: /* fall through */
case 0x2UL: /* fall through */
case 0x3UL: /* fall through */
case 0x4UL: /* fall through */
case 0x0: goto _c1ZK;
case 0x5UL: goto _c1ZM;
case 0x6UL: goto _c1ZO;
}
_c1ZK:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c1ZM:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1ZO:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y3_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
IF_(s1Y3_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c1ZV;
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c1ZV:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s1Y2_info[] = {
0x1UL, 0x22UL
};

II_(s1Y3_info);
II_(s1Y4_info);
II_(s1Y5_info);
II_(s1Y6_info);
II_(s1Y7_info);
II_(s1Y8_info);
II_(s1Y9_info);
IF_(s1Y2_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c1ZY;
case 0x1UL: goto _c200;
case 0x2UL: goto _c202;
case 0x3UL: goto _c204;
case 0x4UL: goto _c206;
case 0x5UL: goto _c208;
case 0x6UL: goto _c20a;
}
_c1ZY:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y9_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20d;
JMP_(*R1.p);
_c20d:
JMP_((W_)&s1Y9_info);
_c200:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y8_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20g;
JMP_(*R1.p);
_c20g:
JMP_((W_)&s1Y8_info);
_c202:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y7_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20j;
JMP_(*R1.p);
_c20j:
JMP_((W_)&s1Y7_info);
_c204:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y6_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20m;
JMP_(*R1.p);
_c20m:
JMP_((W_)&s1Y6_info);
_c206:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y5_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20p;
JMP_(*R1.p);
_c20p:
JMP_((W_)&s1Y5_info);
_c208:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y4_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20s;
JMP_(*R1.p);
_c20s:
JMP_((W_)&s1Y4_info);
_c20a:
R1.w = Sp[1];
Sp[1] = (W_)&s1Y3_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20v;
JMP_(*R1.p);
_c20v:
JMP_((W_)&s1Y3_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_closure);
II_(s1Y2_info);
FN_(Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c20y;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s1Y2_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c20B;
JMP_(*R1.p);
_c20y:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_closure;
JMP_(stg_gc_fun);
_c20B:
JMP_((W_)&s1Y2_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_info
};

static StgWord s20V_info[] = {
0x200000001UL, 0x10UL
};

II_(s14x_info);
IF_(s20V_entry) {
W_ _s20U;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c21C;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s20U = (R1.p[4]) + (R1.p[3]);
R1.w = R1.p[2];
R2.w = _s20U;
Sp=Sp-2;
JMP_((W_)&s14x_info);
_c21C:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20W_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20W_entry) {
W_ _c21H;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c21J;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c21H = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c21H << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c21J:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20T_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20T_entry) {
W_ _c21P;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c21R;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c21P = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c21P << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c21R:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s14x_info[] = {
0x100000004UL, 0x200000000UL, 0xeUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s20T_info);
II_(s20V_info);
II_(s20W_info);
IF_(s14x_entry) {
W_ _c21T;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c21V;
_c21T = (W_)((I_)R2.w < (I_)(*((P_)(R1.w+15))));
if ((W_)(_c21T >= 0x1UL)) goto _c21X;
Hp[-10] = (W_)&s20V_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s20W_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c21V:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
_c21X:
Hp[-10] = (W_)&s20T_info;
Hp[-8] = R2.w;
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Hp=Hp-5;
JMP_(*Sp);
FE_
}

static StgWord s20X_info[] = {
0x300000000UL, 0x10UL
};

II_(s14x_info);
II_(s14x_info);
IF_(s20X_entry) {
W_ _s14p;
W_ _s14r;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c220;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c220;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s14p = (R1.p[2]) - (R1.p[3]);
_s14r = (R1.p[4]) - _s14p;
Hp[-2] = (W_)&s14x_info;
Hp[-1] = _s14p;
*Hp = _s14r;
R2.w = R1.p[2];
R1.w = (W_)Hp-15;
Sp=Sp-2;
JMP_((W_)&s14x_info);
_c220:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20Y_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20Y_entry) {
W_ _c225;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c227;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c225 = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c225 << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c227:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20R_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20R_entry) {
W_ _c22g;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c22i;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c22g = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c22g << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c22i:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20L_info[] = {
0x200000001UL, 0x10UL
};

II_(s14P_info);
IF_(s20L_entry) {
W_ _s20K;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c22D;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s20K = (R1.p[4]) + (R1.p[3]);
R1.w = R1.p[2];
R2.w = _s20K;
Sp=Sp-2;
JMP_((W_)&s14P_info);
_c22D:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20M_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20M_entry) {
W_ _c22I;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c22K;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c22I = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c22I << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c22K:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20J_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20J_entry) {
W_ _c22Q;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c22S;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c22Q = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c22Q << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c22S:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s14P_info[] = {
0x100000004UL, 0x200000000UL, 0xeUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s20J_info);
II_(s20L_info);
II_(s20M_info);
IF_(s14P_entry) {
W_ _c22U;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c22W;
_c22U = (W_)((I_)R2.w > (I_)(*((P_)(R1.w+15))));
if ((W_)(_c22U >= 0x1UL)) goto _c22Y;
Hp[-10] = (W_)&s20L_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s20M_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c22W:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
_c22Y:
Hp[-10] = (W_)&s20J_info;
Hp[-8] = R2.w;
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Hp=Hp-5;
JMP_(*Sp);
FE_
}

static StgWord s20N_info[] = {
0x300000000UL, 0x10UL
};

II_(s14P_info);
II_(s14P_info);
IF_(s20N_entry) {
W_ _s14H;
W_ _s14J;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c231;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c231;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s14H = (R1.p[2]) - (R1.p[3]);
_s14J = (R1.p[4]) - _s14H;
Hp[-2] = (W_)&s14P_info;
Hp[-1] = _s14H;
*Hp = _s14J;
R2.w = R1.p[2];
R1.w = (W_)Hp-15;
Sp=Sp-2;
JMP_((W_)&s14P_info);
_c231:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20O_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20O_entry) {
W_ _c236;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c238;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c236 = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c236 << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c238:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s20H_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s20H_entry) {
W_ _c23h;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c23j;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c23h = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c23h << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c23j:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s14W_info[] = {
0xc2UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s20H_info);
II_(s20N_info);
II_(s20O_info);
II_(s20R_info);
II_(s20X_info);
II_(s20Y_info);
II_(s14W_info);
IF_(s14W_ret) {
W_ _c23l;
W_ _c23m;
W_ _c23n;
W_ _c23o;
W_ _c23p;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c23r;
_c23p = (W_)((I_)(Sp[2]) >= (I_)(Sp[1]));
if ((W_)(_c23p >= 0x1UL)) goto _c23t;
_c23o = (W_)((I_)R1.w > (I_)(Sp[2]));
if ((W_)(_c23o >= 0x1UL)) goto _c23v;
Hp[-10] = (W_)&s20X_info;
Hp[-8] = Sp[2];
Hp[-7] = Sp[1];
Hp[-6] = R1.w;
Hp[-5] = (W_)&s20Y_info;
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c23r:
HpAlloc = 0x58UL;
*Sp = (W_)&s14W_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
_c23t:
_c23n = (W_)((I_)R1.w < (I_)(Sp[2]));
if ((W_)(_c23n >= 0x1UL)) goto _c23x;
Hp[-10] = (W_)&s20N_info;
Hp[-8] = Sp[2];
Hp[-7] = Sp[1];
Hp[-6] = R1.w;
Hp[-5] = (W_)&s20O_info;
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c23x:
_c23m = (W_)((I_)R1.w < (I_)(Sp[1]));
if ((W_)(_c23m >= 0x1UL)) goto _c23z;
Hp[-10] = (W_)&s20H_info;
Hp[-8] = Sp[1];
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Sp=Sp+3;
Hp=Hp-5;
JMP_(*Sp);
_c23z:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-11;
JMP_(*Sp);
_c23v:
_c23l = (W_)((I_)R1.w > (I_)(Sp[1]));
if ((W_)(_c23l >= 0x1UL)) goto _c23B;
Hp[-10] = (W_)&s20R_info;
Hp[-8] = Sp[1];
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Sp=Sp+3;
Hp=Hp-5;
JMP_(*Sp);
_c23B:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-11;
JMP_(*Sp);
FE_
}

static StgWord s14Y_info[] = {
0xc2UL, 0x22UL
};

II_(s14W_info);
IF_(s14Y_ret) {
W_ _c23E;
FB_
Sp[2] = R1.w;
_c23E = (W_)((I_)(Sp[1]) > (I_)R1.w);
if ((W_)(_c23E >= 0x1UL)) goto _c23H;
R1.w = 0x6UL;
JMP_((W_)&s14W_info);
_c23H:
R1.w = 0x0;
JMP_((W_)&s14W_info);
FE_
}

static StgWord s210_info[] = {
0xc2UL, 0x22UL
};

II_(s14Y_info);
IF_(s210_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c23M;
case 0x1UL: goto _c23O;
case 0x2UL: goto _c23Q;
case 0x3UL: goto _c23S;
case 0x4UL: goto _c23U;
case 0x5UL: goto _c23W;
case 0x6UL: goto _c23Y;
}
_c23M:
R1.w = 0x0;
JMP_((W_)&s14Y_info);
_c23O:
R1.w = 0x1UL;
JMP_((W_)&s14Y_info);
_c23Q:
R1.w = 0x2UL;
JMP_((W_)&s14Y_info);
_c23S:
R1.w = 0x3UL;
JMP_((W_)&s14Y_info);
_c23U:
R1.w = 0x4UL;
JMP_((W_)&s14Y_info);
_c23W:
R1.w = 0x5UL;
JMP_((W_)&s14Y_info);
_c23Y:
R1.w = 0x6UL;
JMP_((W_)&s14Y_info);
FE_
}

static StgWord s151_info[] = {
0x1UL, 0x22UL
};

II_(s210_info);
IF_(s151_ret) {
FB_
*Sp = R1.w;
R1.w = Sp[1];
Sp[-1] = (W_)&s210_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c242;
JMP_(*R1.p);
_c242:
JMP_((W_)&s210_info);
FE_
}

static StgWord s211_info[] = {
0x1UL, 0x22UL
};

II_(s151_info);
IF_(s211_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c247;
case 0x1UL: goto _c249;
case 0x2UL: goto _c24b;
case 0x3UL: goto _c24d;
case 0x4UL: goto _c24f;
case 0x5UL: goto _c24h;
case 0x6UL: goto _c24j;
}
_c247:
R1.w = 0x0;
JMP_((W_)&s151_info);
_c249:
R1.w = 0x1UL;
JMP_((W_)&s151_info);
_c24b:
R1.w = 0x2UL;
JMP_((W_)&s151_info);
_c24d:
R1.w = 0x3UL;
JMP_((W_)&s151_info);
_c24f:
R1.w = 0x4UL;
JMP_((W_)&s151_info);
_c24h:
R1.w = 0x5UL;
JMP_((W_)&s151_info);
_c24j:
R1.w = 0x6UL;
JMP_((W_)&s151_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_closure);
II_(s211_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c24m;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s211_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c24p;
JMP_(*R1.p);
_c24m:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_closure;
JMP_(stg_gc_fun);
_c24p:
JMP_((W_)&s211_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info
};

static StgWord s24t_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info);
IF_(s24t_entry) {
W_ _s158;
W_ _s24s;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c24E;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s158 = R1.p[2];
if ((W_)(_s158 != 0x6UL)) goto _c24H;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c24E:
JMP_(stg_gc_enter_1);
_c24H:
_s24s = _s158 + 0x1UL;
R2.w = _s24s;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info);
FE_
}

static StgWord s24u_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s24u_entry) {
W_ _c24M;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c24O;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c24M = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c24M << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c24O:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_closure);
II_(s24t_info);
II_(s24u_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c24R;
Hp[-8] = (W_)&s24t_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s24u_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c24R:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag12_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag12_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag12_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag12_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag12_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c250;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c250;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x0;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo6_info);
_c250:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info
};

static StgWord s254_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info);
IF_(s254_entry) {
W_ _s15f;
W_ _s253;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c25f;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15f = R1.p[2];
if ((W_)(_s15f != 0x6UL)) goto _c25i;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c25f:
JMP_(stg_gc_enter_1);
_c25i:
_s253 = _s15f + 0x1UL;
R2.w = _s253;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info);
FE_
}

static StgWord s255_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s255_entry) {
W_ _c25n;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c25p;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c25n = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c25n << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c25p:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_closure);
II_(s254_info);
II_(s255_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c25s;
Hp[-8] = (W_)&s254_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s255_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c25s:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag11_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag11_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag11_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag11_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag11_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c25B;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c25B;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x1UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo5_info);
_c25B:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info
};

static StgWord s25F_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info);
IF_(s25F_entry) {
W_ _s15m;
W_ _s25E;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c25Q;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15m = R1.p[2];
if ((W_)(_s15m != 0x6UL)) goto _c25T;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c25Q:
JMP_(stg_gc_enter_1);
_c25T:
_s25E = _s15m + 0x1UL;
R2.w = _s25E;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info);
FE_
}

static StgWord s25G_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s25G_entry) {
W_ _c25Y;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c260;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c25Y = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c25Y << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c260:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_closure);
II_(s25F_info);
II_(s25G_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c263;
Hp[-8] = (W_)&s25F_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s25G_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c263:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag10_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag10_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag10_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag10_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag10_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c26c;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c26c;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x2UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo4_info);
_c26c:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info
};

static StgWord s26g_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info);
IF_(s26g_entry) {
W_ _s15t;
W_ _s26f;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c26r;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15t = R1.p[2];
if ((W_)(_s15t != 0x6UL)) goto _c26u;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c26r:
JMP_(stg_gc_enter_1);
_c26u:
_s26f = _s15t + 0x1UL;
R2.w = _s26f;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info);
FE_
}

static StgWord s26h_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s26h_entry) {
W_ _c26z;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c26B;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c26z = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c26z << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c26B:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_closure);
II_(s26g_info);
II_(s26h_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c26E;
Hp[-8] = (W_)&s26g_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s26h_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c26E:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag9_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag9_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag9_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag9_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag9_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c26N;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c26N;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x3UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo3_info);
_c26N:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info
};

static StgWord s26R_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info);
IF_(s26R_entry) {
W_ _s15A;
W_ _s26Q;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c272;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15A = R1.p[2];
if ((W_)(_s15A != 0x6UL)) goto _c275;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c272:
JMP_(stg_gc_enter_1);
_c275:
_s26Q = _s15A + 0x1UL;
R2.w = _s26Q;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info);
FE_
}

static StgWord s26S_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s26S_entry) {
W_ _c27a;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c27c;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c27a = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c27a << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c27c:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_closure);
II_(s26R_info);
II_(s26S_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c27f;
Hp[-8] = (W_)&s26R_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s26S_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c27f:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag8_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag8_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag8_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag8_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag8_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c27o;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c27o;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x4UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo2_info);
_c27o:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info
};

static StgWord s27s_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info);
IF_(s27s_entry) {
W_ _s15H;
W_ _s27r;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c27D;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15H = R1.p[2];
if ((W_)(_s15H != 0x6UL)) goto _c27G;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c27D:
JMP_(stg_gc_enter_1);
_c27G:
_s27r = _s15H + 0x1UL;
R2.w = _s27r;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info);
FE_
}

static StgWord s27t_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s27t_entry) {
W_ _c27L;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c27N;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c27L = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c27L << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c27N:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_closure);
II_(s27s_info);
II_(s27t_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c27Q;
Hp[-8] = (W_)&s27s_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s27t_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c27Q:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag7_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag7_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag7_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag7_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag7_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c27Z;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c27Z;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x5UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo1_info);
_c27Z:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info
};

static StgWord s283_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info);
IF_(s283_entry) {
W_ _s15O;
W_ _s282;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c28e;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s15O = R1.p[2];
if ((W_)(_s15O != 0x6UL)) goto _c28h;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c28e:
JMP_(stg_gc_enter_1);
_c28h:
_s282 = _s15O + 0x1UL;
R2.w = _s282;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info);
FE_
}

static StgWord s284_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Wochentag_closure_tbl);
IF_(s284_entry) {
W_ _c28m;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c28o;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c28m = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Wochentag_closure_tbl + (_c28m << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c28o:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo_closure);
II_(s283_info);
II_(s284_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c28r;
Hp[-8] = (W_)&s283_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s284_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c28r:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag6_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag6_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag6_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag6_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c28A;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c28A;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x6UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzugo_info);
_c28A:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag6_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag7_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag8_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag9_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag10_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag11_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag12_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag6_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag7_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag8_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag9_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag10_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag11_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag12_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_info, 0x0
};

static StgWord s28D_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_srt+0), 0x0, 0x7f00000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag6_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag7_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag8_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag9_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag10_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag11_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag12_closure);
IF_(s28D_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c28O;
case 0x1UL: goto _c28Q;
case 0x2UL: goto _c28S;
case 0x3UL: goto _c28U;
case 0x4UL: goto _c28W;
case 0x5UL: goto _c28Y;
case 0x6UL: goto _c290;
}
_c28O:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag12_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c28Q:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag11_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c28S:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag10_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c28U:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag9_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c28W:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag8_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c28Y:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag7_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c290:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag6_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_srt+0), 0x100000005UL, 0x0, 0x7f0000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_closure);
II_(s28D_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c293;
R1.p=R2.p;
Sp[-1] = (W_)&s28D_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c296;
JMP_(*R1.p);
_c293:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_closure;
JMP_(stg_gc_fun);
_c296:
JMP_((W_)&s28D_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_info
};

static StgWord s299_info[] = {
0x0, 0x22UL
};

IF_(s299_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c29j;
case 0x1UL: goto _c29l;
case 0x2UL: goto _c29n;
case 0x3UL: goto _c29p;
case 0x4UL: goto _c29r;
case 0x5UL: goto _c29t;
case 0x6UL: goto _c29v;
}
_c29j:
R1.w = (W_)&stg_INTLIKE_closure+257;
Sp=Sp+1;
JMP_(*Sp);
_c29l:
R1.w = (W_)&stg_INTLIKE_closure+273;
Sp=Sp+1;
JMP_(*Sp);
_c29n:
R1.w = (W_)&stg_INTLIKE_closure+289;
Sp=Sp+1;
JMP_(*Sp);
_c29p:
R1.w = (W_)&stg_INTLIKE_closure+305;
Sp=Sp+1;
JMP_(*Sp);
_c29r:
R1.w = (W_)&stg_INTLIKE_closure+321;
Sp=Sp+1;
JMP_(*Sp);
_c29t:
R1.w = (W_)&stg_INTLIKE_closure+337;
Sp=Sp+1;
JMP_(*Sp);
_c29v:
R1.w = (W_)&stg_INTLIKE_closure+353;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure);
II_(s299_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c29y;
R1.p=R2.p;
Sp[-1] = (W_)&s299_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c29B;
JMP_(*R1.p);
_c29y:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure;
JMP_(stg_gc_fun);
_c29B:
JMP_((W_)&s299_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum1_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum1_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_info, 0x0
};

static StgWord s29E_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum1_info);
IF_(s29E_ret) {
FB_
R2.w = *((P_)(R1.w+7));
Sp=Sp+1;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure);
II_(s29E_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c29O;
R1.p=R2.p;
Sp[-1] = (W_)&s29E_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c29S;
JMP_(*R1.p);
_c29O:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure;
JMP_(stg_gc_fun);
_c29S:
JMP_((W_)&s29E_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum1_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag5_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum1_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag5_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag5_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_info, 0x0
};

static StgWord s29Y_info[] = {
0x200000001UL, 0x10UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s16g_info);
IF_(s29Y_entry) {
W_ _c2ao;
W_ _s29X;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2aq;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2ao = (W_)((R1.p[4]) == (R1.p[3]));
if ((W_)(_c2ao >= 0x1UL)) goto _c2at;
_s29X = (R1.p[4]) + 0x1UL;
R1.w = R1.p[2];
R2.w = _s29X;
Sp=Sp-2;
JMP_((W_)&s16g_info);
_c2aq:
JMP_(stg_gc_enter_1);
_c2at:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
FE_
}

static StgWord s29Z_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0x100000000UL, 0x100000012UL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum1_info);
IF_(s29Z_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2ay;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = R1.p[2];
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtoEnum1_info);
_c2ay:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s16g_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0x100000004UL, 0x100000000UL, 0x10000000bUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
II_(s29Y_info);
II_(s29Z_info);
IF_(s16g_entry) {
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2aB;
Hp[-10] = (W_)&s29Y_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s29Z_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2aB:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
FE_
}

static StgWord s16j_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0xc2UL, 0x100000022UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s16g_info);
II_(s16j_info);
IF_(s16j_ret) {
W_ _c2aD;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2aF;
_c2aD = (W_)((I_)(Sp[1]) > (I_)R1.w);
if ((W_)(_c2aD >= 0x1UL)) goto _c2aH;
Hp[-1] = (W_)&s16g_info;
*Hp = R1.w;
R1.w = (W_)Hp-7;
R2.w = Sp[1];
Sp=Sp+3;
JMP_((W_)&s16g_info);
_c2aF:
HpAlloc = 0x10UL;
*Sp = (W_)&s16j_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
_c2aH:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-2;
JMP_(*Sp);
FE_
}

static StgWord s2a0_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0xc2UL, 0x100000022UL
};

II_(s16j_info);
IF_(s2a0_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2aM;
case 0x1UL: goto _c2aO;
case 0x2UL: goto _c2aQ;
case 0x3UL: goto _c2aS;
case 0x4UL: goto _c2aU;
case 0x5UL: goto _c2aW;
case 0x6UL: goto _c2aY;
}
_c2aM:
R1.w = 0x0;
JMP_((W_)&s16j_info);
_c2aO:
R1.w = 0x1UL;
JMP_((W_)&s16j_info);
_c2aQ:
R1.w = 0x2UL;
JMP_((W_)&s16j_info);
_c2aS:
R1.w = 0x3UL;
JMP_((W_)&s16j_info);
_c2aU:
R1.w = 0x4UL;
JMP_((W_)&s16j_info);
_c2aW:
R1.w = 0x5UL;
JMP_((W_)&s16j_info);
_c2aY:
R1.w = 0x6UL;
JMP_((W_)&s16j_info);
FE_
}

static StgWord s16m_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0x1UL, 0x100000022UL
};

II_(s2a0_info);
IF_(s16m_ret) {
FB_
*Sp = R1.w;
R1.w = Sp[1];
Sp[-1] = (W_)&s2a0_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2b2;
JMP_(*R1.p);
_c2b2:
JMP_((W_)&s2a0_info);
FE_
}

static StgWord s2a1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0x1UL, 0x100000022UL
};

II_(s16m_info);
IF_(s2a1_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2b7;
case 0x1UL: goto _c2b9;
case 0x2UL: goto _c2bb;
case 0x3UL: goto _c2bd;
case 0x4UL: goto _c2bf;
case 0x5UL: goto _c2bh;
case 0x6UL: goto _c2bj;
}
_c2b7:
R1.w = 0x0;
JMP_((W_)&s16m_info);
_c2b9:
R1.w = 0x1UL;
JMP_((W_)&s16m_info);
_c2bb:
R1.w = 0x2UL;
JMP_((W_)&s16m_info);
_c2bd:
R1.w = 0x3UL;
JMP_((W_)&s16m_info);
_c2bf:
R1.w = 0x4UL;
JMP_((W_)&s16m_info);
_c2bh:
R1.w = 0x5UL;
JMP_((W_)&s16m_info);
_c2bj:
R1.w = 0x6UL;
JMP_((W_)&s16m_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag5_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_srt+0), 0x20000000cUL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag5_closure);
II_(s2a1_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag5_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2bm;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2a1_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2bp;
JMP_(*R1.p);
_c2bm:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_closure;
JMP_(stg_gc_fun);
_c2bp:
JMP_((W_)&s2a1_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum1_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag1_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum1_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag1_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag1_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumWochentag1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag1_srt+0), 0x300000014UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure);
EI_(base_GHCziEnum_zdwzddmenumFromThenTo_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentag1_entry) {
FB_
R5.p=R3.p;
R3.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure+1;
R6.p=R4.p;
R4.p=R2.p;
R2.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure+1;
JMP_((W_)&base_GHCziEnum_zdwzddmenumFromThenTo_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag13_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzupred_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag13_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzupred_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzupred_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzupred_info, 0x0
};

static StgWord s2bx_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzupred_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag13_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
EI_(Aufgabe3ziDatatypes_Dienstag_closure);
EI_(Aufgabe3ziDatatypes_Montag_closure);
IF_(s2bx_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2bJ;
case 0x1UL: goto _c2bL;
case 0x2UL: goto _c2bN;
case 0x3UL: goto _c2bP;
case 0x4UL: goto _c2bR;
case 0x5UL: goto _c2bT;
case 0x6UL: goto _c2bV;
}
_c2bJ:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag13_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c2bL:
R1.w = (W_)&Aufgabe3ziDatatypes_Montag_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2bN:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2bP:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c2bR:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c2bT:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c2bV:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzupred_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzupred_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzupred_closure);
II_(s2bx_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzupred_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2bY;
R1.p=R2.p;
Sp[-1] = (W_)&s2bx_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2c1;
JMP_(*R1.p);
_c2bY:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzupred_closure;
JMP_(stg_gc_fun);
_c2c1:
JMP_((W_)&s2bx_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag14_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentag14_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_info, 0x0
};

static StgWord s2c4_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentag14_closure);
EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
EI_(Aufgabe3ziDatatypes_Dienstag_closure);
IF_(s2c4_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2cf;
case 0x1UL: goto _c2ch;
case 0x2UL: goto _c2cj;
case 0x3UL: goto _c2cl;
case 0x4UL: goto _c2cn;
case 0x5UL: goto _c2cp;
case 0x6UL: goto _c2cr;
}
_c2cf:
R1.w = (W_)&Aufgabe3ziDatatypes_Dienstag_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2ch:
R1.w = (W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c2cj:
R1.w = (W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4;
Sp=Sp+1;
JMP_(*Sp);
_c2cl:
R1.w = (W_)&Aufgabe3ziDatatypes_Freitag_closure+5;
Sp=Sp+1;
JMP_(*Sp);
_c2cn:
R1.w = (W_)&Aufgabe3ziDatatypes_Samstag_closure+6;
Sp=Sp+1;
JMP_(*Sp);
_c2cp:
R1.w = (W_)&Aufgabe3ziDatatypes_Sonntag_closure+7;
Sp=Sp+1;
JMP_(*Sp);
_c2cr:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentag14_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_closure);
II_(s2c4_info);
FN_(Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2cu;
R1.p=R2.p;
Sp[-1] = (W_)&s2c4_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2cx;
JMP_(*R1.p);
_c2cu:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_closure;
JMP_(stg_gc_fun);
_c2cx:
JMP_((W_)&s2c4_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag1_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentag5_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzupred_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_closure);
EI_(base_GHCziEnum_DZCEnum_static_info);
StgWord Aufgabe3ziDatatypes_zdfEnumWochentag_closure[] = {
(W_)&base_GHCziEnum_DZCEnum_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzusucc_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzupred_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzutoEnum_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzufromEnum_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFrom_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentagzuenumFromThen_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag5_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEnumWochentag1_closure+3), 0x0
};
EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_info);
StgWord Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_info
};

static StgWord s2cK_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cK_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2cZ;
case 0x1UL: goto _c2d1;
case 0x2UL: goto _c2d3;
case 0x3UL: goto _c2d5;
case 0x4UL: goto _c2d7;
case 0x5UL: goto _c2d9;
case 0x6UL: goto _c2db;
}
_c2cZ:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2d1:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2d3:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2d5:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2d7:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2d9:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2db:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cJ_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cJ_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c2di;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2di:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cI_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cI_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c2dp;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2dp:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cH_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cH_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x4UL)) goto _c2dw;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2dw:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cG_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cG_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x5UL)) goto _c2dD;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2dD:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cF_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cF_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x6UL)) goto _c2dK;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2dK:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cE_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2cE_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c2dR;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2dR:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2cD_info[] = {
0x1UL, 0x22UL
};

II_(s2cE_info);
II_(s2cF_info);
II_(s2cG_info);
II_(s2cH_info);
II_(s2cI_info);
II_(s2cJ_info);
II_(s2cK_info);
IF_(s2cD_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2dU;
case 0x1UL: goto _c2dW;
case 0x2UL: goto _c2dY;
case 0x3UL: goto _c2e0;
case 0x4UL: goto _c2e2;
case 0x5UL: goto _c2e4;
case 0x6UL: goto _c2e6;
}
_c2dU:
R1.w = Sp[1];
Sp[1] = (W_)&s2cK_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2e9;
JMP_(*R1.p);
_c2e9:
JMP_((W_)&s2cK_info);
_c2dW:
R1.w = Sp[1];
Sp[1] = (W_)&s2cJ_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2ec;
JMP_(*R1.p);
_c2ec:
JMP_((W_)&s2cJ_info);
_c2dY:
R1.w = Sp[1];
Sp[1] = (W_)&s2cI_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2ef;
JMP_(*R1.p);
_c2ef:
JMP_((W_)&s2cI_info);
_c2e0:
R1.w = Sp[1];
Sp[1] = (W_)&s2cH_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2ei;
JMP_(*R1.p);
_c2ei:
JMP_((W_)&s2cH_info);
_c2e2:
R1.w = Sp[1];
Sp[1] = (W_)&s2cG_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2el;
JMP_(*R1.p);
_c2el:
JMP_((W_)&s2cG_info);
_c2e4:
R1.w = Sp[1];
Sp[1] = (W_)&s2cF_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2eo;
JMP_(*R1.p);
_c2eo:
JMP_((W_)&s2cF_info);
_c2e6:
R1.w = Sp[1];
Sp[1] = (W_)&s2cE_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2er;
JMP_(*R1.p);
_c2er:
JMP_((W_)&s2cE_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_closure);
II_(s2cD_info);
FN_(Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2eu;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2cD_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2ex;
JMP_(*R1.p);
_c2eu:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_closure;
JMP_(stg_gc_fun);
_c2ex:
JMP_((W_)&s2cD_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_info);
StgWord Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_info
};

static StgWord s2eH_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eH_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2eV;
case 0x1UL: goto _c2eX;
case 0x2UL: goto _c2eZ;
case 0x3UL: goto _c2f1;
case 0x4UL: goto _c2f3;
case 0x5UL: goto _c2f5;
case 0x6UL: goto _c2f7;
}
_c2eV:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2eX:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2eZ:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2f1:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2f3:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2f5:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2f7:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eG_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eG_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c2fe;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fe:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eF_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eF_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c2fl;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fl:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eE_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eE_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x4UL)) goto _c2fs;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fs:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eD_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eD_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x5UL)) goto _c2fz;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fz:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eC_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eC_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x6UL)) goto _c2fG;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fG:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eB_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2eB_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x7UL)) goto _c2fN;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2fN:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2eA_info[] = {
0x1UL, 0x22UL
};

II_(s2eB_info);
II_(s2eC_info);
II_(s2eD_info);
II_(s2eE_info);
II_(s2eF_info);
II_(s2eG_info);
II_(s2eH_info);
IF_(s2eA_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2fQ;
case 0x1UL: goto _c2fS;
case 0x2UL: goto _c2fU;
case 0x3UL: goto _c2fW;
case 0x4UL: goto _c2fY;
case 0x5UL: goto _c2g0;
case 0x6UL: goto _c2g2;
}
_c2fQ:
R1.w = Sp[1];
Sp[1] = (W_)&s2eH_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2g5;
JMP_(*R1.p);
_c2g5:
JMP_((W_)&s2eH_info);
_c2fS:
R1.w = Sp[1];
Sp[1] = (W_)&s2eG_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2g8;
JMP_(*R1.p);
_c2g8:
JMP_((W_)&s2eG_info);
_c2fU:
R1.w = Sp[1];
Sp[1] = (W_)&s2eF_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2gb;
JMP_(*R1.p);
_c2gb:
JMP_((W_)&s2eF_info);
_c2fW:
R1.w = Sp[1];
Sp[1] = (W_)&s2eE_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2ge;
JMP_(*R1.p);
_c2ge:
JMP_((W_)&s2eE_info);
_c2fY:
R1.w = Sp[1];
Sp[1] = (W_)&s2eD_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2gh;
JMP_(*R1.p);
_c2gh:
JMP_((W_)&s2eD_info);
_c2g0:
R1.w = Sp[1];
Sp[1] = (W_)&s2eC_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2gk;
JMP_(*R1.p);
_c2gk:
JMP_((W_)&s2eC_info);
_c2g2:
R1.w = Sp[1];
Sp[1] = (W_)&s2eB_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2gn;
JMP_(*R1.p);
_c2gn:
JMP_((W_)&s2eB_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_closure);
II_(s2eA_info);
FN_(Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2gq;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2eA_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2gt;
JMP_(*R1.p);
_c2gq:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_closure;
JMP_(stg_gc_fun);
_c2gt:
JMP_((W_)&s2eA_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_closure);
EI_(base_GHCziClasses_DZCEq_static_info);
StgWord Aufgabe3ziDatatypes_zdfEqWochentag_closure[] = {
(W_)&base_GHCziClasses_DZCEq_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzeze_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEqWochentagzuzsze_closure+2), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag1_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag2_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag3_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag4_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag5_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentag6_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_closure);
EI_(Aufgabe3ziDatatypes_zdfEqWochentag_closure);
EI_(base_GHCziClasses_DZCOrd_static_info);
StgWord Aufgabe3ziDatatypes_zdfOrdWochentag_closure[] = {
(W_)&base_GHCziClasses_DZCOrd_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqWochentag_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentagzucompare_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag6_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag5_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag4_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag3_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag2_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdWochentag1_closure+2), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_closure);
StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_srt+0), 0x300000014UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_entry) {
FB_
R2.p=R3.p;
R3.p=R4.p;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_closure, (W_)&base_GHCziShow_showListzuzu4_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowWochentagzushowList_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowList_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_info, 0x0
};

static StgWord s2gK_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x3UL, 0x100000010UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_info);
IF_(s2gK_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2hh;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2hh;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = R1.p[4];
*Hp = R1.p[3];
R2.w = R1.p[2];
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_info);
_c2hh:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2gI_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu1_closure);
II_(s2gK_info);
IF_(s2gI_ret) {
W_ _c2hk;
FB_
_c2hk = R1.w & 0x7UL;
if ((W_)(_c2hk >= 0x2UL)) goto _c2hm;
R1.w = Sp[2];
Sp=Sp+3;
JMP_(*Sp);
_c2hm:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2ho;
Hp[-7] = (W_)&s2gK_info;
Hp[-5] = *((P_)(R1.w+6));
Hp[-4] = *((P_)(R1.w+14));
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu1_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2ho:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s17n_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x100000005UL, 0x1UL, 0x10000000aUL
};

II_(s2gI_info);
IF_(s17n_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2hr;
Sp[-2] = R1.w;
Sp[-1] = *((P_)(R1.w+7));
R1.p=R2.p;
Sp[-3] = (W_)&s2gI_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2hu;
JMP_(*R1.p);
_c2hr:
JMP_(stg_gc_fun);
_c2hu:
JMP_((W_)&s2gI_info);
FE_
}

static StgWord s2gL_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x2UL, 0x100000013UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu2_closure);
II_(s17n_info);
IF_(s2gL_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2hx;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2hx;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-3] = (W_)&base_GHCziShow_showListzuzu2_closure;
Hp[-2] = R1.p[2];
Hp[-1] = (W_)&s17n_info;
*Hp = (W_)Hp-30;
R2.w = R1.p[3];
R1.w = (W_)Hp-7;
Sp=Sp-2;
JMP_((W_)&s17n_info);
_c2hx:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2gM_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x3UL, 0x100000010UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_info);
II_(s2gL_info);
IF_(s2gM_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2hA;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2hA;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s2gL_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[4];
R2.w = R1.p[3];
R3.p=Hp-3;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_info);
_c2hA:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2gH_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x1UL, 0x300000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu3_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
II_(s2gM_info);
IF_(s2gH_ret) {
W_ _c2hD;
FB_
_c2hD = R1.w & 0x7UL;
if ((W_)(_c2hD >= 0x2UL)) goto _c2hF;
R2.w = (W_)&base_GHCziShow_showListzuzu4_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2hF:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2hH;
Hp[-7] = (W_)&s2gM_info;
Hp[-5] = Sp[1];
Hp[-4] = *((P_)(R1.w+6));
Hp[-3] = *((P_)(R1.w+14));
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu3_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+2;
JMP_(*Sp);
_c2hH:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowWochentagzushowList_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_srt+0), 0x20000000cUL, 0x0, 0x30000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowWochentagzushowList_closure);
II_(s2gH_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentagzushowList_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2hK;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2gH_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2hN;
JMP_(*R1.p);
_c2hK:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_closure;
JMP_(stg_gc_fun);
_c2hN:
JMP_((W_)&s2gH_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_closure);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag8_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowWochentag8_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag8_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowWochentag8_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfShowWochentag8_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowWochentag8_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdwshowsPrec2_info);
FN_(Aufgabe3ziDatatypes_zdfShowWochentag8_entry) {
FB_
R3.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec2_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowWochentagzushowList_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentag8_closure);
EI_(Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_closure);
EI_(base_GHCziShow_DZCShow_static_info);
StgWord Aufgabe3ziDatatypes_zdfShowWochentag_closure[] = {
(W_)&base_GHCziShow_DZCShow_static_info, ((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowsPrec_closure+3), ((W_)&Aufgabe3ziDatatypes_zdfShowWochentag8_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfShowWochentagzushowList_closure+2), 0x0
};
EI_(Aufgabe3ziDatatypes_zdfOrdLagerzucompare_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLagerzucompare_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfOrdLagerzucompare_info
};

static StgWord s2i1_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s2i1_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2ih;
case 0x1UL: goto _c2ij;
case 0x2UL: goto _c2il;
}
_c2ih:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2ij:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2il:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2i0_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
EI_(ghczmprim_GHCziOrdering_LT_closure);
IF_(s2i0_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2is;
case 0x1UL: goto _c2iu;
case 0x2UL: goto _c2iw;
}
_c2is:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c2iu:
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2iw:
R1.w = (W_)&ghczmprim_GHCziOrdering_LT_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2hZ_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziOrdering_EQ_closure);
EI_(ghczmprim_GHCziOrdering_GT_closure);
IF_(s2hZ_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c2iD;
R1.w = (W_)&ghczmprim_GHCziOrdering_EQ_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2iD:
R1.w = (W_)&ghczmprim_GHCziOrdering_GT_closure+3;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2hY_info[] = {
0x1UL, 0x22UL
};

II_(s2hZ_info);
II_(s2i0_info);
II_(s2i1_info);
IF_(s2hY_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2iG;
case 0x1UL: goto _c2iI;
case 0x2UL: goto _c2iK;
}
_c2iG:
R1.w = Sp[1];
Sp[1] = (W_)&s2i1_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2iN;
JMP_(*R1.p);
_c2iN:
JMP_((W_)&s2i1_info);
_c2iI:
R1.w = Sp[1];
Sp[1] = (W_)&s2i0_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2iQ;
JMP_(*R1.p);
_c2iQ:
JMP_((W_)&s2i0_info);
_c2iK:
R1.w = Sp[1];
Sp[1] = (W_)&s2hZ_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2iT;
JMP_(*R1.p);
_c2iT:
JMP_((W_)&s2hZ_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfOrdLagerzucompare_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfOrdLagerzucompare_closure);
II_(s2hY_info);
FN_(Aufgabe3ziDatatypes_zdfOrdLagerzucompare_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2iW;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2hY_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2iZ;
JMP_(*R1.p);
_c2iW:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfOrdLagerzucompare_closure;
JMP_(stg_gc_fun);
_c2iZ:
JMP_((W_)&s2hY_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_info
};

static StgWord s2jj_info[] = {
0x200000001UL, 0x10UL
};

II_(s180_info);
IF_(s2jj_entry) {
W_ _s2ji;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2k0;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s2ji = (R1.p[4]) + (R1.p[3]);
R1.w = R1.p[2];
R2.w = _s2ji;
Sp=Sp-2;
JMP_((W_)&s180_info);
_c2k0:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2jk_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2jk_entry) {
W_ _c2k5;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2k7;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2k5 = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2k5 << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2k7:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2jh_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2jh_entry) {
W_ _c2kd;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2kf;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2kd = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2kd << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2kf:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s180_info[] = {
0x100000004UL, 0x200000000UL, 0xeUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s2jh_info);
II_(s2jj_info);
II_(s2jk_info);
IF_(s180_entry) {
W_ _c2kh;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2kj;
_c2kh = (W_)((I_)R2.w < (I_)(*((P_)(R1.w+15))));
if ((W_)(_c2kh >= 0x1UL)) goto _c2kl;
Hp[-10] = (W_)&s2jj_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2jk_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2kj:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
_c2kl:
Hp[-10] = (W_)&s2jh_info;
Hp[-8] = R2.w;
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Hp=Hp-5;
JMP_(*Sp);
FE_
}

static StgWord s2jl_info[] = {
0x300000000UL, 0x10UL
};

II_(s180_info);
II_(s180_info);
IF_(s2jl_entry) {
W_ _s17S;
W_ _s17U;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2ko;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2ko;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s17S = (R1.p[2]) - (R1.p[3]);
_s17U = (R1.p[4]) - _s17S;
Hp[-2] = (W_)&s180_info;
Hp[-1] = _s17S;
*Hp = _s17U;
R2.w = R1.p[2];
R1.w = (W_)Hp-15;
Sp=Sp-2;
JMP_((W_)&s180_info);
_c2ko:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2jm_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2jm_entry) {
W_ _c2kt;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2kv;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2kt = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2kt << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2kv:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2jf_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2jf_entry) {
W_ _c2kE;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2kG;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2kE = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2kE << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2kG:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2j9_info[] = {
0x200000001UL, 0x10UL
};

II_(s18i_info);
IF_(s2j9_entry) {
W_ _s2j8;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2l1;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s2j8 = (R1.p[4]) + (R1.p[3]);
R1.w = R1.p[2];
R2.w = _s2j8;
Sp=Sp-2;
JMP_((W_)&s18i_info);
_c2l1:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2ja_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2ja_entry) {
W_ _c2l6;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2l8;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2l6 = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2l6 << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2l8:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2j7_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2j7_entry) {
W_ _c2le;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2lg;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2le = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2le << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2lg:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s18i_info[] = {
0x100000004UL, 0x200000000UL, 0xeUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s2j7_info);
II_(s2j9_info);
II_(s2ja_info);
IF_(s18i_entry) {
W_ _c2li;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2lk;
_c2li = (W_)((I_)R2.w > (I_)(*((P_)(R1.w+15))));
if ((W_)(_c2li >= 0x1UL)) goto _c2lm;
Hp[-10] = (W_)&s2j9_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2ja_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2lk:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
_c2lm:
Hp[-10] = (W_)&s2j7_info;
Hp[-8] = R2.w;
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Hp=Hp-5;
JMP_(*Sp);
FE_
}

static StgWord s2jb_info[] = {
0x300000000UL, 0x10UL
};

II_(s18i_info);
II_(s18i_info);
IF_(s2jb_entry) {
W_ _s18a;
W_ _s18c;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2lp;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2lp;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s18a = (R1.p[2]) - (R1.p[3]);
_s18c = (R1.p[4]) - _s18a;
Hp[-2] = (W_)&s18i_info;
Hp[-1] = _s18a;
*Hp = _s18c;
R2.w = R1.p[2];
R1.w = (W_)Hp-15;
Sp=Sp-2;
JMP_((W_)&s18i_info);
_c2lp:
HpAlloc = 0x18UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2jc_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2jc_entry) {
W_ _c2lu;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2lw;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2lu = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2lu << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2lw:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2j5_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2j5_entry) {
W_ _c2lF;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2lH;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2lF = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2lF << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2lH:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s18p_info[] = {
0xc2UL, 0x22UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s2j5_info);
II_(s2jb_info);
II_(s2jc_info);
II_(s2jf_info);
II_(s2jl_info);
II_(s2jm_info);
II_(s18p_info);
IF_(s18p_ret) {
W_ _c2lJ;
W_ _c2lK;
W_ _c2lL;
W_ _c2lM;
W_ _c2lN;
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2lP;
_c2lN = (W_)((I_)(Sp[2]) >= (I_)(Sp[1]));
if ((W_)(_c2lN >= 0x1UL)) goto _c2lR;
_c2lM = (W_)((I_)R1.w > (I_)(Sp[2]));
if ((W_)(_c2lM >= 0x1UL)) goto _c2lT;
Hp[-10] = (W_)&s2jl_info;
Hp[-8] = Sp[2];
Hp[-7] = Sp[1];
Hp[-6] = R1.w;
Hp[-5] = (W_)&s2jm_info;
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2lP:
HpAlloc = 0x58UL;
*Sp = (W_)&s18p_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
_c2lR:
_c2lL = (W_)((I_)R1.w < (I_)(Sp[2]));
if ((W_)(_c2lL >= 0x1UL)) goto _c2lV;
Hp[-10] = (W_)&s2jb_info;
Hp[-8] = Sp[2];
Hp[-7] = Sp[1];
Hp[-6] = R1.w;
Hp[-5] = (W_)&s2jc_info;
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2lV:
_c2lK = (W_)((I_)R1.w < (I_)(Sp[1]));
if ((W_)(_c2lK >= 0x1UL)) goto _c2lX;
Hp[-10] = (W_)&s2j5_info;
Hp[-8] = Sp[1];
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Sp=Sp+3;
Hp=Hp-5;
JMP_(*Sp);
_c2lX:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-11;
JMP_(*Sp);
_c2lT:
_c2lJ = (W_)((I_)R1.w > (I_)(Sp[1]));
if ((W_)(_c2lJ >= 0x1UL)) goto _c2lZ;
Hp[-10] = (W_)&s2jf_info;
Hp[-8] = Sp[1];
Hp[-7] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-6] = (W_)Hp-80;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)Hp-54;
Sp=Sp+3;
Hp=Hp-5;
JMP_(*Sp);
_c2lZ:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-11;
JMP_(*Sp);
FE_
}

static StgWord s18r_info[] = {
0xc2UL, 0x22UL
};

II_(s18p_info);
IF_(s18r_ret) {
W_ _c2m2;
FB_
Sp[2] = R1.w;
_c2m2 = (W_)((I_)(Sp[1]) > (I_)R1.w);
if ((W_)(_c2m2 >= 0x1UL)) goto _c2m5;
R1.w = 0x2UL;
JMP_((W_)&s18p_info);
_c2m5:
R1.w = 0x0;
JMP_((W_)&s18p_info);
FE_
}

static StgWord s2jo_info[] = {
0xc2UL, 0x22UL
};

II_(s18r_info);
IF_(s2jo_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2ma;
case 0x1UL: goto _c2mc;
case 0x2UL: goto _c2me;
}
_c2ma:
R1.w = 0x0;
JMP_((W_)&s18r_info);
_c2mc:
R1.w = 0x1UL;
JMP_((W_)&s18r_info);
_c2me:
R1.w = 0x2UL;
JMP_((W_)&s18r_info);
FE_
}

static StgWord s18u_info[] = {
0x1UL, 0x22UL
};

II_(s2jo_info);
IF_(s18u_ret) {
FB_
*Sp = R1.w;
R1.w = Sp[1];
Sp[-1] = (W_)&s2jo_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2mi;
JMP_(*R1.p);
_c2mi:
JMP_((W_)&s2jo_info);
FE_
}

static StgWord s2jp_info[] = {
0x1UL, 0x22UL
};

II_(s18u_info);
IF_(s2jp_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2mn;
case 0x1UL: goto _c2mp;
case 0x2UL: goto _c2mr;
}
_c2mn:
R1.w = 0x0;
JMP_((W_)&s18u_info);
_c2mp:
R1.w = 0x1UL;
JMP_((W_)&s18u_info);
_c2mr:
R1.w = 0x2UL;
JMP_((W_)&s18u_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_closure);
II_(s2jp_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2mu;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2jp_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2mx;
JMP_(*R1.p);
_c2mu:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_closure;
JMP_(stg_gc_fun);
_c2mx:
JMP_((W_)&s2jp_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo2_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info
};

static StgWord s2mB_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info);
IF_(s2mB_entry) {
W_ _s18B;
W_ _s2mA;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2mM;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s18B = R1.p[2];
if ((W_)(_s18B != 0x2UL)) goto _c2mP;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2mM:
JMP_(stg_gc_enter_1);
_c2mP:
_s2mA = _s18B + 0x1UL;
R2.w = _s2mA;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info);
FE_
}

static StgWord s2mC_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2mC_entry) {
W_ _c2mU;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2mW;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2mU = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2mU << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2mW:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo2_closure);
II_(s2mB_info);
II_(s2mC_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzugo2_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2mZ;
Hp[-8] = (W_)&s2mB_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2mC_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2mZ:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo2_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager10_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager10_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager10_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager10_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager10_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2n8;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2n8;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x0;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo2_info);
_c2n8:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info
};

static StgWord s2nc_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info);
IF_(s2nc_entry) {
W_ _s18I;
W_ _s2nb;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2nn;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s18I = R1.p[2];
if ((W_)(_s18I != 0x2UL)) goto _c2nq;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2nn:
JMP_(stg_gc_enter_1);
_c2nq:
_s2nb = _s18I + 0x1UL;
R2.w = _s2nb;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info);
FE_
}

static StgWord s2nd_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2nd_entry) {
W_ _c2nv;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2nx;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2nv = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2nv << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2nx:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo1_closure);
II_(s2nc_info);
II_(s2nd_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzugo1_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2nA;
Hp[-8] = (W_)&s2nc_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2nd_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2nA:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo1_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager9_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager9_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager9_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager9_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager9_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2nJ;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2nJ;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x1UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo1_info);
_c2nJ:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo_info
};

static StgWord s2nN_info[] = {
0x100000000UL, 0x12UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo_info);
IF_(s2nN_entry) {
W_ _s18P;
W_ _s2nM;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2nY;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_s18P = R1.p[2];
if ((W_)(_s18P != 0x2UL)) goto _c2o1;
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2nY:
JMP_(stg_gc_enter_1);
_c2o1:
_s2nM = _s18P + 0x1UL;
R2.w = _s2nM;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo_info);
FE_
}

static StgWord s2nO_info[] = {
0x100000000UL, 0x12UL
};

EI_(Aufgabe3ziDatatypes_Lager_closure_tbl);
IF_(s2nO_entry) {
W_ _c2o6;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2o8;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2o6 = R1.p[2];
R1.w = *((P_)((W_)&Aufgabe3ziDatatypes_Lager_closure_tbl + (_c2o6 << 0x3UL)));
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
_c2o8:
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzugo_info[] = {
0x100000004UL, 0x0, 0xfUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo_closure);
II_(s2nN_info);
II_(s2nO_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzugo_entry) {
FB_
Hp=Hp+9;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2ob;
Hp[-8] = (W_)&s2nN_info;
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2nO_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-64;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2ob:
HpAlloc = 0x48UL;
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager8_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager8_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager8_info, 0x0, 0x0, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager8_info[] = {
0x0, 0x16UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzugo_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager8_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2ok;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2ok;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EF_(newCAF);
{void (*ghcFunPtr)(void *);
ghcFunPtr = ((void (*)(void *))(W_)&newCAF);
__DISCARD__();
ghcFunPtr((void *)R1.w);;}
R1.p[1] = (W_)Hp-8;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-8;
R2.w = 0x2UL;
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzugo_info);
_c2ok:
HpAlloc = 0x10UL;
JMP_(stg_gc_enter_1);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager8_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager9_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager10_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager8_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumLager9_closure, (W_)&Aufgabe3ziDatatypes_zdfEnumLager10_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_info, 0x0
};

static StgWord s2on_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_srt+0), 0x0, 0x700000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager8_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager9_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager10_closure);
IF_(s2on_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2oy;
case 0x1UL: goto _c2oA;
case 0x2UL: goto _c2oC;
}
_c2oy:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager10_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c2oA:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager9_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c2oC:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager8_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_srt+0), 0x100000005UL, 0x0, 0x70000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_closure);
II_(s2on_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2oF;
R1.p=R2.p;
Sp[-1] = (W_)&s2on_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2oI;
JMP_(*R1.p);
_c2oF:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_closure;
JMP_(stg_gc_fun);
_c2oI:
JMP_((W_)&s2on_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_info
};

static StgWord s2oL_info[] = {
0x0, 0x22UL
};

IF_(s2oL_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2oV;
case 0x1UL: goto _c2oX;
case 0x2UL: goto _c2oZ;
}
_c2oV:
R1.w = (W_)&stg_INTLIKE_closure+257;
Sp=Sp+1;
JMP_(*Sp);
_c2oX:
R1.w = (W_)&stg_INTLIKE_closure+273;
Sp=Sp+1;
JMP_(*Sp);
_c2oZ:
R1.w = (W_)&stg_INTLIKE_closure+289;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_info[] = {
0x100000005UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure);
II_(s2oL_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2p2;
R1.p=R2.p;
Sp[-1] = (W_)&s2oL_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2p5;
JMP_(*R1.p);
_c2p2:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure;
JMP_(stg_gc_fun);
_c2p5:
JMP_((W_)&s2oL_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_info, 0x0
};

static StgWord s2p8_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum_info);
IF_(s2p8_ret) {
FB_
R2.w = *((P_)(R1.w+7));
Sp=Sp+1;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtoEnum_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure);
II_(s2p8_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2pi;
R1.p=R2.p;
Sp[-1] = (W_)&s2p8_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2pm;
JMP_(*R1.p);
_c2pi:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure;
JMP_(stg_gc_fun);
_c2pm:
JMP_((W_)&s2p8_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager7_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager7_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager7_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager7_info, 0x0
};

static StgWord s2ps_info[] = {
0x200000001UL, 0x10UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s19h_info);
IF_(s2ps_entry) {
W_ _c2pS;
W_ _s2pr;
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2pU;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
_c2pS = (W_)((R1.p[4]) == (R1.p[3]));
if ((W_)(_c2pS >= 0x1UL)) goto _c2pX;
_s2pr = (R1.p[4]) + 0x1UL;
R1.w = R1.p[2];
R2.w = _s2pr;
Sp=Sp-2;
JMP_((W_)&s19h_info);
_c2pU:
JMP_(stg_gc_enter_1);
_c2pX:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp-2;
JMP_((W_)&stg_upd_frame_info);
FE_
}

static StgWord s2pt_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0x100000000UL, 0x100000012UL
};

EI_(Aufgabe3ziDatatypes_zdwtoEnum_info);
IF_(s2pt_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2q2;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
R2.w = R1.p[2];
Sp=Sp-2;
JMP_((W_)&Aufgabe3ziDatatypes_zdwtoEnum_info);
_c2q2:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s19h_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0x100000004UL, 0x100000000UL, 0x10000000bUL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
II_(s2ps_info);
II_(s2pt_info);
IF_(s19h_entry) {
FB_
Hp=Hp+11;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2q5;
Hp[-10] = (W_)&s2ps_info;
Hp[-8] = R1.w;
Hp[-7] = *((P_)(R1.w+7));
Hp[-6] = R2.w;
Hp[-5] = (W_)&s2pt_info;
Hp[-3] = R2.w;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)Hp-40;
*Hp = (W_)Hp-80;
R1.w = (W_)Hp-14;
JMP_(*Sp);
_c2q5:
HpAlloc = 0x58UL;
JMP_(stg_gc_fun);
FE_
}

static StgWord s19k_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0xc2UL, 0x100000022UL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(s19h_info);
II_(s19k_info);
IF_(s19k_ret) {
W_ _c2q7;
FB_
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2q9;
_c2q7 = (W_)((I_)(Sp[1]) > (I_)R1.w);
if ((W_)(_c2q7 >= 0x1UL)) goto _c2qb;
Hp[-1] = (W_)&s19h_info;
*Hp = R1.w;
R1.w = (W_)Hp-7;
R2.w = Sp[1];
Sp=Sp+3;
JMP_((W_)&s19h_info);
_c2q9:
HpAlloc = 0x10UL;
*Sp = (W_)&s19k_info;
R9.w = 0xffUL;
JMP_((W_)&stg_gc_ut);
_c2qb:
R1.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Sp=Sp+3;
Hp=Hp-2;
JMP_(*Sp);
FE_
}

static StgWord s2pu_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0xc2UL, 0x100000022UL
};

II_(s19k_info);
IF_(s2pu_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2qg;
case 0x1UL: goto _c2qi;
case 0x2UL: goto _c2qk;
}
_c2qg:
R1.w = 0x0;
JMP_((W_)&s19k_info);
_c2qi:
R1.w = 0x1UL;
JMP_((W_)&s19k_info);
_c2qk:
R1.w = 0x2UL;
JMP_((W_)&s19k_info);
FE_
}

static StgWord s19n_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0x1UL, 0x100000022UL
};

II_(s2pu_info);
IF_(s19n_ret) {
FB_
*Sp = R1.w;
R1.w = Sp[1];
Sp[-1] = (W_)&s2pu_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2qo;
JMP_(*R1.p);
_c2qo:
JMP_((W_)&s2pu_info);
FE_
}

static StgWord s2pv_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0x1UL, 0x100000022UL
};

II_(s19n_info);
IF_(s2pv_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2qt;
case 0x1UL: goto _c2qv;
case 0x2UL: goto _c2qx;
}
_c2qt:
R1.w = 0x0;
JMP_((W_)&s19n_info);
_c2qv:
R1.w = 0x1UL;
JMP_((W_)&s19n_info);
_c2qx:
R1.w = 0x2UL;
JMP_((W_)&s19n_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLager7_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_srt+0), 0x20000000cUL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager7_closure);
II_(s2pv_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLager7_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2qA;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2pv_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2qD;
JMP_(*R1.p);
_c2qA:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager7_closure;
JMP_(stg_gc_fun);
_c2qD:
JMP_((W_)&s2pv_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwtoEnum_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLager1_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwtoEnum_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager1_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager1_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfEnumLager1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLager1_srt+0), 0x300000014UL, 0x0, 0x10000000fUL
};

EI_(base_GHCziEnum_zdwzddmenumFromThenTo_info);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure);
FN_(Aufgabe3ziDatatypes_zdfEnumLager1_entry) {
FB_
R5.p=R3.p;
R3.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure+1;
R6.p=R4.p;
R4.p=R2.p;
R2.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure+1;
JMP_((W_)&base_GHCziEnum_zdwzddmenumFromThenTo_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager11_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzupred_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager11_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzupred_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzupred_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzupred_info, 0x0
};

static StgWord s2qL_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzupred_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager11_closure);
EI_(Aufgabe3ziDatatypes_LagerB_closure);
EI_(Aufgabe3ziDatatypes_LagerA_closure);
IF_(s2qL_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2qX;
case 0x1UL: goto _c2qZ;
case 0x2UL: goto _c2r1;
}
_c2qX:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager11_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
_c2qZ:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerA_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2r1:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerB_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzupred_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzupred_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzupred_closure);
II_(s2qL_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzupred_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2r4;
R1.p=R2.p;
Sp[-1] = (W_)&s2qL_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2r7;
JMP_(*R1.p);
_c2r4:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzupred_closure;
JMP_(stg_gc_fun);
_c2r7:
JMP_((W_)&s2qL_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager12_closure);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzusucc_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLager12_closure
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzusucc_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLagerzusucc_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEnumLagerzusucc_info, 0x0
};

static StgWord s2ra_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzusucc_srt+0), 0x0, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLager12_closure);
EI_(Aufgabe3ziDatatypes_LagerC_closure);
EI_(Aufgabe3ziDatatypes_LagerB_closure);
IF_(s2ra_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2rl;
case 0x1UL: goto _c2rn;
case 0x2UL: goto _c2rp;
}
_c2rl:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerB_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2rn:
R1.w = (W_)&Aufgabe3ziDatatypes_LagerC_closure+3;
Sp=Sp+1;
JMP_(*Sp);
_c2rp:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLager12_closure;
Sp=Sp+1;
R1.w = R1.w & (-0x8UL);
JMP_(*R1.p);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEnumLagerzusucc_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzusucc_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfEnumLagerzusucc_closure);
II_(s2ra_info);
FN_(Aufgabe3ziDatatypes_zdfEnumLagerzusucc_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2rs;
R1.p=R2.p;
Sp[-1] = (W_)&s2ra_info;
Sp=Sp-1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2rv;
JMP_(*R1.p);
_c2rs:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEnumLagerzusucc_closure;
JMP_(stg_gc_fun);
_c2rv:
JMP_((W_)&s2ra_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEnumLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLager7_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzupred_closure);
EI_(Aufgabe3ziDatatypes_zdfEnumLagerzusucc_closure);
EI_(base_GHCziEnum_DZCEnum_static_info);
StgWord Aufgabe3ziDatatypes_zdfEnumLager_closure[] = {
(W_)&base_GHCziEnum_DZCEnum_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzusucc_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzupred_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzutoEnum_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzufromEnum_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFrom_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfEnumLagerzuenumFromThen_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEnumLager7_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEnumLager1_closure+3), 0x0
};
EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzsze_info);
StgWord Aufgabe3ziDatatypes_zdfEqLagerzuzsze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzsze_info
};

static StgWord s2rE_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2rE_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2rT;
case 0x1UL: goto _c2rV;
case 0x2UL: goto _c2rX;
}
_c2rT:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2rV:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2rX:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2rD_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2rD_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c2s4;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2s4:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2rC_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2rC_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c2sb;
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2sb:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2rB_info[] = {
0x1UL, 0x22UL
};

II_(s2rC_info);
II_(s2rD_info);
II_(s2rE_info);
IF_(s2rB_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2se;
case 0x1UL: goto _c2sg;
case 0x2UL: goto _c2si;
}
_c2se:
R1.w = Sp[1];
Sp[1] = (W_)&s2rE_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2sl;
JMP_(*R1.p);
_c2sl:
JMP_((W_)&s2rE_info);
_c2sg:
R1.w = Sp[1];
Sp[1] = (W_)&s2rD_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2so;
JMP_(*R1.p);
_c2so:
JMP_((W_)&s2rD_info);
_c2si:
R1.w = Sp[1];
Sp[1] = (W_)&s2rC_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2sr;
JMP_(*R1.p);
_c2sr:
JMP_((W_)&s2rC_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqLagerzuzsze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzsze_closure);
II_(s2rB_info);
FN_(Aufgabe3ziDatatypes_zdfEqLagerzuzsze_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2su;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2rB_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2sx;
JMP_(*R1.p);
_c2su:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzsze_closure;
JMP_(stg_gc_fun);
_c2sx:
JMP_((W_)&s2rB_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzeze_info);
StgWord Aufgabe3ziDatatypes_zdfEqLagerzuzeze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzeze_info
};

static StgWord s2sD_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2sD_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2sR;
case 0x1UL: goto _c2sT;
case 0x2UL: goto _c2sV;
}
_c2sR:
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2sT:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
_c2sV:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2sC_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2sC_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x2UL)) goto _c2t2;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2t2:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2sB_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2sB_ret) {
FB_
if ((W_)((R1.w & 0x7UL) != 0x3UL)) goto _c2t9;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2t9:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

static StgWord s2sA_info[] = {
0x1UL, 0x22UL
};

II_(s2sB_info);
II_(s2sC_info);
II_(s2sD_info);
IF_(s2sA_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2tc;
case 0x1UL: goto _c2te;
case 0x2UL: goto _c2tg;
}
_c2tc:
R1.w = Sp[1];
Sp[1] = (W_)&s2sD_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2tj;
JMP_(*R1.p);
_c2tj:
JMP_((W_)&s2sD_info);
_c2te:
R1.w = Sp[1];
Sp[1] = (W_)&s2sC_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2tm;
JMP_(*R1.p);
_c2tm:
JMP_((W_)&s2sC_info);
_c2tg:
R1.w = Sp[1];
Sp[1] = (W_)&s2sB_info;
Sp=Sp+1;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2tp;
JMP_(*R1.p);
_c2tp:
JMP_((W_)&s2sB_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqLagerzuzeze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzeze_closure);
II_(s2sA_info);
FN_(Aufgabe3ziDatatypes_zdfEqLagerzuzeze_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2ts;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2sA_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2tv;
JMP_(*R1.p);
_c2ts:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzeze_closure;
JMP_(stg_gc_fun);
_c2tv:
JMP_((W_)&s2sA_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqLagerzuzeze_closure);
EI_(base_GHCziClasses_DZCEq_static_info);
StgWord Aufgabe3ziDatatypes_zdfEqLager_closure[] = {
(W_)&base_GHCziClasses_DZCEq_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzeze_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEqLagerzuzsze_closure+2), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdfOrdLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLager3_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLager4_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLager5_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLager6_closure);
EI_(Aufgabe3ziDatatypes_zdfOrdLagerzucompare_closure);
EI_(Aufgabe3ziDatatypes_zdfEqLager_closure);
EI_(base_GHCziClasses_DZCOrd_static_info);
StgWord Aufgabe3ziDatatypes_zdfOrdLager_closure[] = {
(W_)&base_GHCziClasses_DZCOrd_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqLager_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfOrdLagerzucompare_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager6_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager5_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager4_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager3_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager2_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfOrdLager1_closure+2), 0x1UL
};
EI_(base_GHCziShow_showListzuzu4_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt[] = {
(W_)&base_GHCziShow_showListzuzu4_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowList_info);
StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowList_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_info, 0x0
};

static StgWord s2tG_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x2UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s2tG_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2ui;
case 0x1UL: goto _c2uk;
case 0x2UL: goto _c2um;
}
_c2ui:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2uq;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = Sp[1];
*Hp = Sp[2];
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
R3.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2uq:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c2uk:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2uu;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = Sp[1];
*Hp = Sp[2];
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
R3.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2uu:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c2um:
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2uy;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = Sp[1];
*Hp = Sp[2];
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
R3.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2uy:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2tK_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x3UL, 0x700000010UL
};

II_(s2tG_info);
IF_(s2tK_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c2uB;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-5] = (W_)&s2tG_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2uE;
JMP_(*R1.p);
_c2uB:
JMP_(stg_gc_enter_1);
_c2uE:
JMP_((W_)&s2tG_info);
FE_
}

static StgWord s2tF_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x2UL, 0x700000022UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu1_closure);
II_(s2tK_info);
IF_(s2tF_ret) {
W_ _c2uH;
FB_
_c2uH = R1.w & 0x7UL;
if ((W_)(_c2uH >= 0x2UL)) goto _c2uJ;
R1.w = Sp[2];
Sp=Sp+3;
JMP_(*Sp);
_c2uJ:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2uL;
Hp[-7] = (W_)&s2tK_info;
Hp[-5] = *((P_)(R1.w+6));
Hp[-4] = *((P_)(R1.w+14));
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu1_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2uL:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1ab_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x100000005UL, 0x1UL, 0x70000000aUL
};

II_(s2tF_info);
IF_(s1ab_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2uO;
Sp[-2] = R1.w;
Sp[-1] = *((P_)(R1.w+7));
R1.p=R2.p;
Sp[-3] = (W_)&s2tF_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2uR;
JMP_(*R1.p);
_c2uO:
JMP_(stg_gc_fun);
_c2uR:
JMP_((W_)&s2tF_info);
FE_
}

static StgWord s1ah_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x2UL, 0x700000013UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu2_closure);
II_(s1ab_info);
IF_(s1ah_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2uU;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2uU;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-3] = (W_)&base_GHCziShow_showListzuzu2_closure;
Hp[-2] = R1.p[2];
Hp[-1] = (W_)&s1ab_info;
*Hp = (W_)Hp-30;
R2.w = R1.p[3];
R1.w = (W_)Hp-7;
Sp=Sp-2;
JMP_((W_)&s1ab_info);
_c2uU:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2tL_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x1UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s2tL_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2uZ;
case 0x1UL: goto _c2v1;
case 0x2UL: goto _c2v3;
}
_c2uZ:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2v1:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2v3:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
FE_
}

static StgWord s2tM_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+8), 0x3UL, 0x700000010UL
};

II_(s1ah_info);
II_(s2tL_info);
IF_(s2tM_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c2v6;
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2v6;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-3] = (W_)&s1ah_info;
Hp[-1] = R1.p[2];
*Hp = R1.p[3];
Sp[-3] = (W_)Hp-24;
R1.w = R1.p[4];
Sp[-4] = (W_)&s2tL_info;
Sp=Sp-4;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2v9;
JMP_(*R1.p);
_c2v6:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
_c2v9:
JMP_((W_)&s2tL_info);
FE_
}

static StgWord s2tE_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+0), 0x1UL, 0xf00000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu3_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
II_(s2tM_info);
IF_(s2tE_ret) {
W_ _c2vc;
FB_
_c2vc = R1.w & 0x7UL;
if ((W_)(_c2vc >= 0x2UL)) goto _c2ve;
R2.w = (W_)&base_GHCziShow_showListzuzu4_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2ve:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2vg;
Hp[-7] = (W_)&s2tM_info;
Hp[-5] = Sp[1];
Hp[-4] = *((P_)(R1.w+14));
Hp[-3] = *((P_)(R1.w+6));
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu3_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+2;
JMP_(*Sp);
_c2vg:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowList_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_srt+0), 0x20000000cUL, 0x0, 0xf0000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowList_closure);
II_(s2tE_info);
FN_(Aufgabe3ziDatatypes_zdfShowLagerzushowList_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2vj;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2tE_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2vm;
JMP_(*R1.p);
_c2vj:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_closure;
JMP_(stg_gc_fun);
_c2vm:
JMP_((W_)&s2tE_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure, (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_info);
StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_info, 0x0
};

static StgWord s2vp_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_srt+0), 0x1UL, 0x700000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(Aufgabe3ziDatatypes_zdfShowLager1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager2_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager3_closure);
IF_(s2vp_ret) {
FB_
switch ((R1.w & 0x7UL) + (-0x1UL)) {
case 0x0: goto _c2vA;
case 0x1UL: goto _c2vC;
case 0x2UL: goto _c2vE;
}
_c2vA:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager3_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2vC:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager2_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2vE:
R2.w = (W_)&Aufgabe3ziDatatypes_zdfShowLager1_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_srt+0), 0x300000014UL, 0x0, 0x70000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_closure);
II_(s2vp_info);
FN_(Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2vH;
Sp[-1] = R4.w;
R1.p=R3.p;
Sp[-2] = (W_)&s2vp_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2vK;
JMP_(*R1.p);
_c2vH:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_closure;
JMP_(stg_gc_fun);
_c2vK:
JMP_((W_)&s2vp_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowList_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLager4_closure);
EI_(Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_closure);
EI_(base_GHCziShow_DZCShow_static_info);
StgWord Aufgabe3ziDatatypes_zdfShowLager_closure[] = {
(W_)&base_GHCziShow_DZCShow_static_info, ((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowsPrec_closure+3), ((W_)&Aufgabe3ziDatatypes_zdfShowLager4_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfShowLagerzushowList_closure+2), 0x0
};
EI_(Aufgabe3ziDatatypes_zdfEqTourzuzeze_info);
StgWord Aufgabe3ziDatatypes_zdfEqTourzuzeze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqTourzuzeze_info
};

static StgWord s2vR_info[] = {
0x3UL, 0x22UL
};

EI_(Aufgabe3ziDatatypes_zdwzeze_info);
IF_(s2vR_ret) {
FB_
R2.w = Sp[3];
Sp[3] = *((P_)(R1.w+23));
R3.w = Sp[2];
R4.w = Sp[1];
R5.w = *((P_)(R1.w+7));
R6.w = *((P_)(R1.w+15));
Sp=Sp+3;
JMP_((W_)&Aufgabe3ziDatatypes_zdwzeze_info);
FE_
}

static StgWord s2vQ_info[] = {
0x1UL, 0x22UL
};

II_(s2vR_info);
IF_(s2vQ_ret) {
W_ _c2w7;
FB_
Sp[-1] = *((P_)(R1.w+23));
*Sp = *((P_)(R1.w+15));
_c2w7 = Sp[1];
Sp[1] = *((P_)(R1.w+7));
R1.w = _c2w7;
Sp[-2] = (W_)&s2vR_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2w9;
JMP_(*R1.p);
_c2w9:
JMP_((W_)&s2vR_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqTourzuzeze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqTourzuzeze_closure);
II_(s2vQ_info);
FN_(Aufgabe3ziDatatypes_zdfEqTourzuzeze_entry) {
FB_
if ((W_)(((W_)Sp - 0x20UL) < (W_)SpLim)) goto _c2wc;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2vQ_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2wf;
JMP_(*R1.p);
_c2wc:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqTourzuzeze_closure;
JMP_(stg_gc_fun);
_c2wf:
JMP_((W_)&s2vQ_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqTourzuzsze_info);
StgWord Aufgabe3ziDatatypes_zdfEqTourzuzsze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqTourzuzsze_info
};

static StgWord s2wi_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2wi_ret) {
W_ _c2ws;
FB_
_c2ws = R1.w & 0x7UL;
if ((W_)(_c2ws >= 0x2UL)) goto _c2wu;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2wu:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqTourzuzsze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqTourzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqTourzuzeze_info);
II_(s2wi_info);
FN_(Aufgabe3ziDatatypes_zdfEqTourzuzsze_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2wx;
Sp[-1] = (W_)&s2wi_info;
Sp=Sp-1;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEqTourzuzeze_info);
_c2wx:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqTourzuzsze_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqTourzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqTourzuzeze_closure);
EI_(base_GHCziClasses_DZCEq_static_info);
StgWord Aufgabe3ziDatatypes_zdfEqTour_closure[] = {
(W_)&base_GHCziClasses_DZCEq_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqTourzuzeze_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEqTourzuzsze_closure+2), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdwshowsPrec_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_info, 0x0
};

static StgWord s2wE_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_srt+0), 0x82UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec_info);
IF_(s2wE_ret) {
FB_
R2.w = Sp[2];
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.w = Sp[1];
Sp=Sp+3;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_info);
FE_
}

static StgWord s2wD_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_srt+0), 0x2UL, 0x100000022UL
};

II_(s2wE_info);
IF_(s2wD_ret) {
W_ _c2wV;
FB_
_c2wV = Sp[2];
Sp[2] = *((P_)(R1.w+7));
R1.w = _c2wV;
*Sp = (W_)&s2wE_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2wX;
JMP_(*R1.p);
_c2wX:
JMP_((W_)&s2wE_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_srt+0), 0x300000014UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_closure);
II_(s2wD_info);
FN_(Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2x0;
Sp[-2] = R4.w;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-3] = (W_)&s2wD_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2x3;
JMP_(*R1.p);
_c2x0:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_closure;
JMP_(stg_gc_fun);
_c2x3:
JMP_((W_)&s2wD_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTourzushowList_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec_closure, (W_)&base_GHCziShow_showListzuzu4_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTourzushowList_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourzushowList_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_info, 0x0
};

static StgWord s2x9_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec_info);
IF_(s2x9_ret) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2xN;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = Sp[1];
*Hp = Sp[2];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_info);
_c2xN:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2xb_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x3UL, 0x100000010UL
};

II_(s2x9_info);
IF_(s2xb_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c2xQ;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-5] = (W_)&s2x9_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2xT;
JMP_(*R1.p);
_c2xQ:
JMP_(stg_gc_enter_1);
_c2xT:
JMP_((W_)&s2x9_info);
FE_
}

static StgWord s2x8_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu1_closure);
II_(s2xb_info);
IF_(s2x8_ret) {
W_ _c2xW;
FB_
_c2xW = R1.w & 0x7UL;
if ((W_)(_c2xW >= 0x2UL)) goto _c2xY;
R1.w = Sp[2];
Sp=Sp+3;
JMP_(*Sp);
_c2xY:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2y0;
Hp[-7] = (W_)&s2xb_info;
Hp[-5] = *((P_)(R1.w+6));
Hp[-4] = *((P_)(R1.w+14));
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu1_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2y0:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1bB_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x100000005UL, 0x1UL, 0x10000000aUL
};

II_(s2x8_info);
IF_(s1bB_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2y3;
Sp[-2] = R1.w;
Sp[-1] = *((P_)(R1.w+7));
R1.p=R2.p;
Sp[-3] = (W_)&s2x8_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2y6;
JMP_(*R1.p);
_c2y3:
JMP_(stg_gc_fun);
_c2y6:
JMP_((W_)&s2x8_info);
FE_
}

static StgWord s2xc_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x2UL, 0x100000013UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu2_closure);
II_(s1bB_info);
IF_(s2xc_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2y9;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2y9;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-3] = (W_)&base_GHCziShow_showListzuzu2_closure;
Hp[-2] = R1.p[2];
Hp[-1] = (W_)&s1bB_info;
*Hp = (W_)Hp-30;
R2.w = R1.p[3];
R1.w = (W_)Hp-7;
Sp=Sp-2;
JMP_((W_)&s1bB_info);
_c2y9:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2x7_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec_info);
II_(s2xc_info);
IF_(s2x7_ret) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2yc;
Hp[-3] = (W_)&s2xc_info;
Hp[-1] = Sp[2];
*Hp = Sp[1];
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.p=Hp-3;
Sp=Sp+3;
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec_info);
_c2yc:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2xd_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x3UL, 0x100000010UL
};

II_(s2x7_info);
IF_(s2xd_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c2yf;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[2];
R1.w = R1.p[3];
Sp[-5] = (W_)&s2x7_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2yi;
JMP_(*R1.p);
_c2yf:
JMP_(stg_gc_enter_1);
_c2yi:
JMP_((W_)&s2x7_info);
FE_
}

static StgWord s2x6_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x1UL, 0x300000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu3_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
II_(s2xd_info);
IF_(s2x6_ret) {
W_ _c2yl;
FB_
_c2yl = R1.w & 0x7UL;
if ((W_)(_c2yl >= 0x2UL)) goto _c2yn;
R2.w = (W_)&base_GHCziShow_showListzuzu4_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2yn:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2yp;
Hp[-7] = (W_)&s2xd_info;
Hp[-5] = Sp[1];
Hp[-4] = *((P_)(R1.w+6));
Hp[-3] = *((P_)(R1.w+14));
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu3_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+2;
JMP_(*Sp);
_c2yp:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowTourzushowList_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_srt+0), 0x20000000cUL, 0x0, 0x30000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowTourzushowList_closure);
II_(s2x6_info);
FN_(Aufgabe3ziDatatypes_zdfShowTourzushowList_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2ys;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2x6_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2yv;
JMP_(*R1.p);
_c2ys:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_closure;
JMP_(stg_gc_fun);
_c2yv:
JMP_((W_)&s2x6_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTour1_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTour1_info);
StgWord Aufgabe3ziDatatypes_zdfShowTour1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTour1_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfShowTour1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTour1_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(base_GHCziBase_zzeroInt_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_info);
FN_(Aufgabe3ziDatatypes_zdfShowTour1_entry) {
FB_
R3.p=R2.p;
R2.w = (W_)&base_GHCziBase_zzeroInt_closure;
R4.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
JMP_((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowTourzushowList_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTour1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_closure);
EI_(base_GHCziShow_DZCShow_static_info);
StgWord Aufgabe3ziDatatypes_zdfShowTour_closure[] = {
(W_)&base_GHCziShow_DZCShow_static_info, ((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowsPrec_closure+3), ((W_)&Aufgabe3ziDatatypes_zdfShowTour1_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfShowTourzushowList_closure+2), 0x0
};
EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_info);
StgWord Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_info
};

static StgWord s2yH_info[] = {
0x6UL, 0x22UL
};

EI_(Aufgabe3ziDatatypes_zdwzeze1_info);
IF_(s2yH_ret) {
FB_
R2.w = *((P_)(R1.w+7));
R3.w = Sp[5];
Sp[5] = Sp[1];
R4.w = Sp[4];
R5.w = Sp[3];
R6.w = Sp[2];
Sp=Sp+5;
JMP_((W_)&Aufgabe3ziDatatypes_zdwzeze1_info);
FE_
}

static StgWord s2yG_info[] = {
0x1UL, 0x22UL
};

II_(s2yH_info);
IF_(s2yG_ret) {
FB_
Sp[-4] = *((P_)(R1.w+47));
Sp[-3] = *((P_)(R1.w+39));
Sp[-2] = *((P_)(R1.w+31));
Sp[-1] = *((P_)(R1.w+23));
*Sp = *((P_)(R1.w+15));
R1.w = *((P_)(R1.w+7));
Sp[-5] = (W_)&s2yH_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2yY;
JMP_(*R1.p);
_c2yY:
JMP_((W_)&s2yH_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_closure);
II_(s2yG_info);
FN_(Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_entry) {
FB_
if ((W_)(((W_)Sp - 0x38UL) < (W_)SpLim)) goto _c2z1;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2yG_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2z4;
JMP_(*R1.p);
_c2z1:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_closure;
JMP_(stg_gc_fun);
_c2z4:
JMP_((W_)&s2yG_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_info);
StgWord Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_info
};

static StgWord s2z7_info[] = {
0x0, 0x22UL
};

EI_(ghczmprim_GHCziBool_False_closure);
EI_(ghczmprim_GHCziBool_True_closure);
IF_(s2z7_ret) {
W_ _c2zh;
FB_
_c2zh = R1.w & 0x7UL;
if ((W_)(_c2zh >= 0x2UL)) goto _c2zj;
R1.w = (W_)&ghczmprim_GHCziBool_True_closure+2;
Sp=Sp+1;
JMP_(*Sp);
_c2zj:
R1.w = (W_)&ghczmprim_GHCziBool_False_closure+1;
Sp=Sp+1;
JMP_(*Sp);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_info[] = {
0x20000000cUL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_info);
II_(s2z7_info);
FN_(Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_entry) {
FB_
if ((W_)(((W_)Sp - 0x8UL) < (W_)SpLim)) goto _c2zm;
Sp[-1] = (W_)&s2z7_info;
Sp=Sp-1;
JMP_((W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_info);
_c2zm:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_closure);
EI_(Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_closure);
EI_(base_GHCziClasses_DZCEq_static_info);
StgWord Aufgabe3ziDatatypes_zdfEqTourSet_closure[] = {
(W_)&base_GHCziClasses_DZCEq_static_info, ((W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzeze_closure+2), ((W_)&Aufgabe3ziDatatypes_zdfEqTourSetzuzsze_closure+2), 0x1UL
};
EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_info, 0x0
};

static StgWord s2zt_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_srt+0), 0x82UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_info);
IF_(s2zt_ret) {
FB_
*Sp = *((P_)(R1.w+39));
R2.w = Sp[2];
Sp[2] = Sp[1];
Sp[1] = *((P_)(R1.w+47));
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.w = *((P_)(R1.w+31));
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_info);
FE_
}

static StgWord s2zs_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_srt+0), 0x2UL, 0x100000022UL
};

II_(s2zt_info);
IF_(s2zs_ret) {
W_ _c2zK;
FB_
_c2zK = Sp[2];
Sp[2] = *((P_)(R1.w+7));
R1.w = _c2zK;
*Sp = (W_)&s2zt_info;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2zM;
JMP_(*R1.p);
_c2zM:
JMP_((W_)&s2zt_info);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_srt+0), 0x300000014UL, 0x0, 0x10000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_closure);
II_(s2zs_info);
FN_(Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2zP;
Sp[-2] = R4.w;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-3] = (W_)&s2zs_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2zS;
JMP_(*R1.p);
_c2zP:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_closure;
JMP_(stg_gc_fun);
_c2zS:
JMP_((W_)&s2zs_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_closure, (W_)&base_GHCziShow_showListzuzu4_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowList_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowList_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_info, 0x0
};

static StgWord s2zY_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_info);
IF_(s2zY_ret) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2AC;
Hp[-3] = (W_)&stg_ap_2_upd_info;
Hp[-1] = Sp[1];
*Hp = Sp[2];
Sp[2] = (W_)Hp-24;
Sp[1] = *((P_)(R1.w+47));
*Sp = *((P_)(R1.w+39));
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.w = *((P_)(R1.w+31));
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_info);
_c2AC:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2A0_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x3UL, 0x100000010UL
};

II_(s2zY_info);
IF_(s2A0_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c2AF;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[3];
R1.w = R1.p[2];
Sp[-5] = (W_)&s2zY_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2AI;
JMP_(*R1.p);
_c2AF:
JMP_(stg_gc_enter_1);
_c2AI:
JMP_((W_)&s2zY_info);
FE_
}

static StgWord s2zX_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu1_closure);
II_(s2A0_info);
IF_(s2zX_ret) {
W_ _c2AL;
FB_
_c2AL = R1.w & 0x7UL;
if ((W_)(_c2AL >= 0x2UL)) goto _c2AN;
R1.w = Sp[2];
Sp=Sp+3;
JMP_(*Sp);
_c2AN:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2AP;
Hp[-7] = (W_)&s2A0_info;
Hp[-5] = *((P_)(R1.w+6));
Hp[-4] = *((P_)(R1.w+14));
Hp[-3] = Sp[1];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu1_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+3;
JMP_(*Sp);
_c2AP:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s1dc_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x100000005UL, 0x1UL, 0x10000000aUL
};

II_(s2zX_info);
IF_(s1dc_entry) {
FB_
if ((W_)(((W_)Sp - 0x18UL) < (W_)SpLim)) goto _c2AS;
Sp[-2] = R1.w;
Sp[-1] = *((P_)(R1.w+7));
R1.p=R2.p;
Sp[-3] = (W_)&s2zX_info;
Sp=Sp-3;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2AV;
JMP_(*R1.p);
_c2AS:
JMP_(stg_gc_fun);
_c2AV:
JMP_((W_)&s2zX_info);
FE_
}

static StgWord s2A1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x2UL, 0x100000013UL
};

EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu2_closure);
II_(s1dc_info);
IF_(s2A1_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2AY;
Hp=Hp+5;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2AY;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-4] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-3] = (W_)&base_GHCziShow_showListzuzu2_closure;
Hp[-2] = R1.p[2];
Hp[-1] = (W_)&s1dc_info;
*Hp = (W_)Hp-30;
R2.w = R1.p[3];
R1.w = (W_)Hp-7;
Sp=Sp-2;
JMP_((W_)&s1dc_info);
_c2AY:
HpAlloc = 0x28UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2zW_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x2UL, 0x100000022UL
};

EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_info);
II_(s2A1_info);
IF_(s2zW_ret) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2B1;
Hp[-3] = (W_)&s2A1_info;
Hp[-1] = Sp[2];
*Hp = Sp[1];
Sp[2] = (W_)Hp-24;
Sp[1] = *((P_)(R1.w+47));
*Sp = *((P_)(R1.w+39));
R2.w = 0x0;
R3.w = *((P_)(R1.w+7));
R4.w = *((P_)(R1.w+15));
R5.w = *((P_)(R1.w+23));
R6.w = *((P_)(R1.w+31));
JMP_((W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_info);
_c2B1:
HpAlloc = 0x20UL;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord s2A2_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x3UL, 0x100000010UL
};

II_(s2zW_info);
IF_(s2A2_entry) {
FB_
if ((W_)(((W_)Sp - 0x28UL) < (W_)SpLim)) goto _c2B4;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-4] = R1.p[4];
Sp[-3] = R1.p[2];
R1.w = R1.p[3];
Sp[-5] = (W_)&s2zW_info;
Sp=Sp-5;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2B7;
JMP_(*R1.p);
_c2B4:
JMP_(stg_gc_enter_1);
_c2B7:
JMP_((W_)&s2zW_info);
FE_
}

static StgWord s2zV_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x1UL, 0x300000022UL
};

EI_(base_GHCziBase_zpzp_info);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(base_GHCziShow_showListzuzu3_closure);
EI_(base_GHCziShow_showListzuzu4_closure);
II_(s2A2_info);
IF_(s2zV_ret) {
W_ _c2Ba;
FB_
_c2Ba = R1.w & 0x7UL;
if ((W_)(_c2Ba >= 0x2UL)) goto _c2Bc;
R2.w = (W_)&base_GHCziShow_showListzuzu4_closure;
R3.w = Sp[1];
Sp=Sp+2;
JMP_((W_)&base_GHCziBase_zpzp_info);
_c2Bc:
Hp=Hp+8;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2Be;
Hp[-7] = (W_)&s2A2_info;
Hp[-5] = Sp[1];
Hp[-4] = *((P_)(R1.w+6));
Hp[-3] = *((P_)(R1.w+14));
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&base_GHCziShow_showListzuzu3_closure;
*Hp = (W_)Hp-56;
R1.w = (W_)Hp-14;
Sp=Sp+2;
JMP_(*Sp);
_c2Be:
HpAlloc = 0x40UL;
JMP_(stg_gc_enter_1);
FE_
}

StgWord Aufgabe3ziDatatypes_zdfShowTourSetzushowList_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_srt+0), 0x20000000cUL, 0x0, 0x30000000fUL
};

EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowList_closure);
II_(s2zV_info);
FN_(Aufgabe3ziDatatypes_zdfShowTourSetzushowList_entry) {
FB_
if ((W_)(((W_)Sp - 0x10UL) < (W_)SpLim)) goto _c2Bh;
Sp[-1] = R3.w;
R1.p=R2.p;
Sp[-2] = (W_)&s2zV_info;
Sp=Sp-2;
if ((W_)((R1.w & 0x7UL) != 0x0)) goto _c2Bk;
JMP_(*R1.p);
_c2Bh:
R1.w = (W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_closure;
JMP_(stg_gc_fun);
_c2Bk:
JMP_((W_)&s2zV_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdwshowsPrec1_closure);
StgWord Aufgabe3ziDatatypes_zdfShowTourSet1_srt[] = {
(W_)&Aufgabe3ziDatatypes_zdwshowsPrec1_closure
};

EI_(Aufgabe3ziDatatypes_zdfShowTourSet1_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourSet1_closure[] = {
(W_)&Aufgabe3ziDatatypes_zdfShowTourSet1_info, 0x0
};

StgWord Aufgabe3ziDatatypes_zdfShowTourSet1_info[] = {
((W_)&Aufgabe3ziDatatypes_zdfShowTourSet1_srt+0), 0x100000005UL, 0x0, 0x10000000fUL
};

EI_(ghczmprim_GHCziTypes_ZMZN_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_info);
EI_(base_GHCziBase_zzeroInt_closure);
FN_(Aufgabe3ziDatatypes_zdfShowTourSet1_entry) {
FB_
R3.p=R2.p;
R2.w = (W_)&base_GHCziBase_zzeroInt_closure;
R4.w = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
JMP_((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_info);
FE_
}
EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowList_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTourSet1_closure);
EI_(Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_closure);
EI_(base_GHCziShow_DZCShow_static_info);
StgWord Aufgabe3ziDatatypes_zdfShowTourSet_closure[] = {
(W_)&base_GHCziShow_DZCShow_static_info, ((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowsPrec_closure+3), ((W_)&Aufgabe3ziDatatypes_zdfShowTourSet1_closure+1), ((W_)&Aufgabe3ziDatatypes_zdfShowTourSetzushowList_closure+2), 0x0
};
EI_(Aufgabe3ziDatatypes_TourSet_info);
StgWord Aufgabe3ziDatatypes_TourSet_closure[] = {
(W_)&Aufgabe3ziDatatypes_TourSet_info
};

StgWord Aufgabe3ziDatatypes_TourSet_info[] = {
0x600000017UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_TourSet_con_info);
EI_(Aufgabe3ziDatatypes_TourSet_closure);
FN_(Aufgabe3ziDatatypes_TourSet_entry) {
FB_
Hp=Hp+7;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2BE;
Hp[-6] = (W_)&Aufgabe3ziDatatypes_TourSet_con_info;
Hp[-5] = R2.w;
Hp[-4] = R3.w;
Hp[-3] = R4.w;
Hp[-2] = R5.w;
Hp[-1] = R6.w;
*Hp = *Sp;
R1.w = (W_)Hp-47;
Sp=Sp+1;
JMP_(*Sp);
_c2BE:
HpAlloc = 0x38UL;
R1.w = (W_)&Aufgabe3ziDatatypes_TourSet_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_Tour_info);
StgWord Aufgabe3ziDatatypes_Tour_closure[] = {
(W_)&Aufgabe3ziDatatypes_Tour_info
};

StgWord Aufgabe3ziDatatypes_Tour_info[] = {
0x300000014UL, 0x0, 0xfUL
};

EI_(Aufgabe3ziDatatypes_Tour_con_info);
EI_(Aufgabe3ziDatatypes_Tour_closure);
FN_(Aufgabe3ziDatatypes_Tour_entry) {
FB_
Hp=Hp+4;
if ((W_)((W_)Hp > (W_)HpLim)) goto _c2BO;
Hp[-3] = (W_)&Aufgabe3ziDatatypes_Tour_con_info;
Hp[-2] = R2.w;
Hp[-1] = R3.w;
*Hp = R4.w;
R1.w = (W_)Hp-23;
JMP_(*Sp);
_c2BO:
HpAlloc = 0x20UL;
R1.w = (W_)&Aufgabe3ziDatatypes_Tour_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(Aufgabe3ziDatatypes_Montag_static_info);
StgWord Aufgabe3ziDatatypes_Montag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Montag_static_info
};
EI_(Aufgabe3ziDatatypes_Dienstag_static_info);
StgWord Aufgabe3ziDatatypes_Dienstag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Dienstag_static_info
};
EI_(Aufgabe3ziDatatypes_Mittwoch_static_info);
StgWord Aufgabe3ziDatatypes_Mittwoch_closure[] = {
(W_)&Aufgabe3ziDatatypes_Mittwoch_static_info
};
EI_(Aufgabe3ziDatatypes_Donnerstag_static_info);
StgWord Aufgabe3ziDatatypes_Donnerstag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Donnerstag_static_info
};
EI_(Aufgabe3ziDatatypes_Freitag_static_info);
StgWord Aufgabe3ziDatatypes_Freitag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Freitag_static_info
};
EI_(Aufgabe3ziDatatypes_Samstag_static_info);
StgWord Aufgabe3ziDatatypes_Samstag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Samstag_static_info
};
EI_(Aufgabe3ziDatatypes_Sonntag_static_info);
StgWord Aufgabe3ziDatatypes_Sonntag_closure[] = {
(W_)&Aufgabe3ziDatatypes_Sonntag_static_info
};
EI_(Aufgabe3ziDatatypes_LagerA_static_info);
StgWord Aufgabe3ziDatatypes_LagerA_closure[] = {
(W_)&Aufgabe3ziDatatypes_LagerA_static_info
};
EI_(Aufgabe3ziDatatypes_LagerB_static_info);
StgWord Aufgabe3ziDatatypes_LagerB_closure[] = {
(W_)&Aufgabe3ziDatatypes_LagerB_static_info
};
EI_(Aufgabe3ziDatatypes_LagerC_static_info);
StgWord Aufgabe3ziDatatypes_LagerC_closure[] = {
(W_)&Aufgabe3ziDatatypes_LagerC_static_info
};
static char c2Cy_str[] = "main:Aufgabe3.Datatypes.TourSet";

StgWord Aufgabe3ziDatatypes_TourSet_con_info[] = {
((W_)&c2Cy_str+0), 0x6UL, 0x1UL
};

FN_(Aufgabe3ziDatatypes_TourSet_con_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}

static char c2CD_str[] = "main:Aufgabe3.Datatypes.TourSet";

StgWord Aufgabe3ziDatatypes_TourSet_static_info[] = {
((W_)&c2CD_str+0), 0x6UL, 0x7UL
};

FN_(Aufgabe3ziDatatypes_TourSet_static_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}
static char c2CK_str[] = "main:Aufgabe3.Datatypes.Tour";

StgWord Aufgabe3ziDatatypes_Tour_con_info[] = {
((W_)&c2CK_str+0), 0x3UL, 0x1UL
};

FN_(Aufgabe3ziDatatypes_Tour_con_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}

static char c2CP_str[] = "main:Aufgabe3.Datatypes.Tour";

StgWord Aufgabe3ziDatatypes_Tour_static_info[] = {
((W_)&c2CP_str+0), 0x3UL, 0x7UL
};

FN_(Aufgabe3ziDatatypes_Tour_static_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}
EI_(Aufgabe3ziDatatypes_Sonntag_closure);
EI_(Aufgabe3ziDatatypes_Samstag_closure);
EI_(Aufgabe3ziDatatypes_Freitag_closure);
EI_(Aufgabe3ziDatatypes_Donnerstag_closure);
EI_(Aufgabe3ziDatatypes_Mittwoch_closure);
EI_(Aufgabe3ziDatatypes_Dienstag_closure);
EI_(Aufgabe3ziDatatypes_Montag_closure);
StgWord Aufgabe3ziDatatypes_Wochentag_closure_tbl[] = {
((W_)&Aufgabe3ziDatatypes_Montag_closure+1), ((W_)&Aufgabe3ziDatatypes_Dienstag_closure+2), ((W_)&Aufgabe3ziDatatypes_Mittwoch_closure+3), ((W_)&Aufgabe3ziDatatypes_Donnerstag_closure+4), ((W_)&Aufgabe3ziDatatypes_Freitag_closure+5), ((W_)&Aufgabe3ziDatatypes_Samstag_closure+6), ((W_)&Aufgabe3ziDatatypes_Sonntag_closure+7)
};
static char c2CZ_str[] = "main:Aufgabe3.Datatypes.Montag";

StgWord Aufgabe3ziDatatypes_Montag_static_info[] = {
((W_)&c2CZ_str+0), 0x0, 0x8UL
};

FN_(Aufgabe3ziDatatypes_Montag_static_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}
static char c2D6_str[] = "main:Aufgabe3.Datatypes.Dienstag";

StgWord Aufgabe3ziDatatypes_Dienstag_static_info[] = {
((W_)&c2D6_str+0), 0x0, 0x100000008UL
};

FN_(Aufgabe3ziDatatypes_Dienstag_static_entry) {
FB_
R1.w = R1.w+2;
JMP_(*Sp);
FE_
}
static char c2Dd_str[] = "main:Aufgabe3.Datatypes.Mittwoch";

StgWord Aufgabe3ziDatatypes_Mittwoch_static_info[] = {
((W_)&c2Dd_str+0), 0x0, 0x200000008UL
};

FN_(Aufgabe3ziDatatypes_Mittwoch_static_entry) {
FB_
R1.w = R1.w+3;
JMP_(*Sp);
FE_
}
static char c2Dk_str[] = "main:Aufgabe3.Datatypes.Donnerstag";

StgWord Aufgabe3ziDatatypes_Donnerstag_static_info[] = {
((W_)&c2Dk_str+0), 0x0, 0x300000008UL
};

FN_(Aufgabe3ziDatatypes_Donnerstag_static_entry) {
FB_
R1.w = R1.w+4;
JMP_(*Sp);
FE_
}
static char c2Dr_str[] = "main:Aufgabe3.Datatypes.Freitag";

StgWord Aufgabe3ziDatatypes_Freitag_static_info[] = {
((W_)&c2Dr_str+0), 0x0, 0x400000008UL
};

FN_(Aufgabe3ziDatatypes_Freitag_static_entry) {
FB_
R1.w = R1.w+5;
JMP_(*Sp);
FE_
}
static char c2Dy_str[] = "main:Aufgabe3.Datatypes.Samstag";

StgWord Aufgabe3ziDatatypes_Samstag_static_info[] = {
((W_)&c2Dy_str+0), 0x0, 0x500000008UL
};

FN_(Aufgabe3ziDatatypes_Samstag_static_entry) {
FB_
R1.w = R1.w+6;
JMP_(*Sp);
FE_
}
static char c2DF_str[] = "main:Aufgabe3.Datatypes.Sonntag";

StgWord Aufgabe3ziDatatypes_Sonntag_static_info[] = {
((W_)&c2DF_str+0), 0x0, 0x600000008UL
};

FN_(Aufgabe3ziDatatypes_Sonntag_static_entry) {
FB_
R1.w = R1.w+7;
JMP_(*Sp);
FE_
}
EI_(Aufgabe3ziDatatypes_LagerC_closure);
EI_(Aufgabe3ziDatatypes_LagerB_closure);
EI_(Aufgabe3ziDatatypes_LagerA_closure);
StgWord Aufgabe3ziDatatypes_Lager_closure_tbl[] = {
((W_)&Aufgabe3ziDatatypes_LagerA_closure+1), ((W_)&Aufgabe3ziDatatypes_LagerB_closure+2), ((W_)&Aufgabe3ziDatatypes_LagerC_closure+3)
};
static char c2DP_str[] = "main:Aufgabe3.Datatypes.LagerA";

StgWord Aufgabe3ziDatatypes_LagerA_static_info[] = {
((W_)&c2DP_str+0), 0x0, 0x8UL
};

FN_(Aufgabe3ziDatatypes_LagerA_static_entry) {
FB_
R1.w = R1.w+1;
JMP_(*Sp);
FE_
}
static char c2DW_str[] = "main:Aufgabe3.Datatypes.LagerB";

StgWord Aufgabe3ziDatatypes_LagerB_static_info[] = {
((W_)&c2DW_str+0), 0x0, 0x100000008UL
};

FN_(Aufgabe3ziDatatypes_LagerB_static_entry) {
FB_
R1.w = R1.w+2;
JMP_(*Sp);
FE_
}
static char c2E3_str[] = "main:Aufgabe3.Datatypes.LagerC";

StgWord Aufgabe3ziDatatypes_LagerC_static_info[] = {
((W_)&c2E3_str+0), 0x0, 0x200000008UL
};

FN_(Aufgabe3ziDatatypes_LagerC_static_entry) {
FB_
R1.w = R1.w+3;
JMP_(*Sp);
FE_
}
static StgWord _module_registered[] = {
0x0
};


EF_(__stginit_base_ControlziException_);
EF_(__stginit_base_Prelude_);
FN_(__stginit_Aufgabe3ziDatatypes_) {
FB_
if ((W_)(0x0 != (*((P_)(W_)&_module_registered)))) goto _c2Ea;
goto _c2Ec;
_c2Ea:
Sp=Sp+1;
JMP_(Sp[-1]);
_c2Ec:
*((P_)(W_)&_module_registered) = 0x1UL;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_ControlziException_;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_Prelude_;
goto _c2Ea;
FE_
}


EF_(__stginit_Aufgabe3ziDatatypes_);
FN_(__stginit_Aufgabe3ziDatatypes) {
FB_
JMP_((W_)&__stginit_Aufgabe3ziDatatypes_);
FE_
}
