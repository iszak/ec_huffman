#[macro_use]
extern crate bencher;
extern crate ec;

use bencher::{black_box, Bencher};
use std::collections::HashMap;

fn bench_encode_decode(b: &mut Bencher) {
    let mut map: HashMap<&str, usize> = HashMap::new();
    map.insert("AA", 54);
    map.insert("AB", 9);
    map.insert("AC", 27);
    map.insert("AD", 69);
    map.insert("AE", 88);
    map.insert("AF", 74);
    map.insert("AG", 44);
    map.insert("AH", 61);
    map.insert("AI", 36);
    map.insert("AJ", 54);
    map.insert("AK", 81);
    map.insert("AL", 22);
    map.insert("AM", 11);
    map.insert("AN", 32);
    map.insert("AO", 79);
    map.insert("AP", 21);
    map.insert("AQ", 49);
    map.insert("AR", 8);
    map.insert("AS", 68);
    map.insert("AT", 40);
    map.insert("AU", 95);
    map.insert("AV", 99);
    map.insert("AW", 87);
    map.insert("AX", 67);
    map.insert("AY", 75);
    map.insert("AZ", 35);
    map.insert("BA", 39);
    map.insert("BB", 91);
    map.insert("BC", 15);
    map.insert("BD", 19);
    map.insert("BE", 33);
    map.insert("BF", 84);
    map.insert("BG", 89);
    map.insert("BH", 59);
    map.insert("BI", 15);
    map.insert("BJ", 66);
    map.insert("BK", 2);
    map.insert("BL", 29);
    map.insert("BM", 42);
    map.insert("BN", 29);
    map.insert("BO", 37);
    map.insert("BP", 23);
    map.insert("BQ", 98);
    map.insert("BR", 1);
    map.insert("BS", 66);
    map.insert("BT", 98);
    map.insert("BU", 44);
    map.insert("BV", 70);
    map.insert("BW", 58);
    map.insert("BX", 52);
    map.insert("BY", 87);
    map.insert("BZ", 67);
    map.insert("CA", 58);
    map.insert("CB", 40);
    map.insert("CC", 52);
    map.insert("CD", 51);
    map.insert("CE", 99);
    map.insert("CF", 15);
    map.insert("CG", 39);
    map.insert("CH", 81);
    map.insert("CI", 3);
    map.insert("CJ", 75);
    map.insert("CK", 4);
    map.insert("CL", 15);
    map.insert("CM", 91);
    map.insert("CN", 39);
    map.insert("CO", 47);
    map.insert("CP", 62);
    map.insert("CQ", 77);
    map.insert("CR", 55);
    map.insert("CS", 38);
    map.insert("CT", 18);
    map.insert("CU", 60);
    map.insert("CV", 76);
    map.insert("CW", 75);
    map.insert("CX", 19);
    map.insert("CY", 16);
    map.insert("CZ", 49);
    map.insert("DA", 80);
    map.insert("DB", 20);
    map.insert("DC", 59);
    map.insert("DD", 93);
    map.insert("DE", 46);
    map.insert("DF", 57);
    map.insert("DG", 3);
    map.insert("DH", 72);
    map.insert("DI", 4);
    map.insert("DJ", 19);
    map.insert("DK", 47);
    map.insert("DL", 85);
    map.insert("DM", 4);
    map.insert("DN", 28);
    map.insert("DO", 65);
    map.insert("DP", 93);
    map.insert("DQ", 5);
    map.insert("DR", 11);
    map.insert("DS", 11);
    map.insert("DT", 45);
    map.insert("DU", 15);
    map.insert("DV", 12);
    map.insert("DW", 48);
    map.insert("DX", 4);
    map.insert("DY", 13);
    map.insert("DZ", 30);
    map.insert("EA", 29);
    map.insert("EB", 37);
    map.insert("EC", 89);
    map.insert("ED", 100);
    map.insert("EE", 75);
    map.insert("EF", 99);
    map.insert("EG", 59);
    map.insert("EH", 23);
    map.insert("EI", 74);
    map.insert("EJ", 45);
    map.insert("EK", 63);
    map.insert("EL", 68);
    map.insert("EM", 27);
    map.insert("EN", 60);
    map.insert("EO", 10);
    map.insert("EP", 84);
    map.insert("EQ", 29);
    map.insert("ER", 37);
    map.insert("ES", 4);
    map.insert("ET", 94);
    map.insert("EU", 84);
    map.insert("EV", 68);
    map.insert("EW", 19);
    map.insert("EX", 88);
    map.insert("EY", 78);
    map.insert("EZ", 15);
    map.insert("FA", 21);
    map.insert("FB", 93);
    map.insert("FC", 52);
    map.insert("FD", 97);
    map.insert("FE", 4);
    map.insert("FF", 34);
    map.insert("FG", 58);
    map.insert("FH", 50);
    map.insert("FI", 74);
    map.insert("FJ", 77);
    map.insert("FK", 4);
    map.insert("FL", 84);
    map.insert("FM", 55);
    map.insert("FN", 60);
    map.insert("FO", 67);
    map.insert("FP", 35);
    map.insert("FQ", 59);
    map.insert("FR", 42);
    map.insert("FS", 66);
    map.insert("FT", 98);
    map.insert("FU", 11);
    map.insert("FV", 82);
    map.insert("FW", 100);
    map.insert("FX", 28);
    map.insert("FY", 37);
    map.insert("FZ", 15);
    map.insert("GA", 62);
    map.insert("GB", 6);
    map.insert("GC", 27);
    map.insert("GD", 37);
    map.insert("GE", 28);
    map.insert("GF", 79);
    map.insert("GG", 96);
    map.insert("GH", 63);
    map.insert("GI", 78);
    map.insert("GJ", 11);
    map.insert("GK", 73);
    map.insert("GL", 36);
    map.insert("GM", 40);
    map.insert("GN", 67);
    map.insert("GO", 31);
    map.insert("GP", 32);
    map.insert("GQ", 79);
    map.insert("GR", 90);
    map.insert("GS", 13);
    map.insert("GT", 27);
    map.insert("GU", 56);
    map.insert("GV", 17);
    map.insert("GW", 98);
    map.insert("GX", 42);
    map.insert("GY", 68);
    map.insert("GZ", 11);
    map.insert("HA", 76);
    map.insert("HB", 22);
    map.insert("HC", 69);
    map.insert("HD", 91);
    map.insert("HE", 79);
    map.insert("HF", 39);
    map.insert("HG", 82);
    map.insert("HH", 27);
    map.insert("HI", 40);
    map.insert("HJ", 71);
    map.insert("HK", 18);
    map.insert("HL", 20);
    map.insert("HM", 88);
    map.insert("HN", 43);
    map.insert("HO", 35);
    map.insert("HP", 89);
    map.insert("HQ", 13);
    map.insert("HR", 71);
    map.insert("HS", 69);
    map.insert("HT", 50);
    map.insert("HU", 70);
    map.insert("HV", 33);
    map.insert("HW", 71);
    map.insert("HX", 15);
    map.insert("HY", 46);
    map.insert("HZ", 96);
    map.insert("IA", 52);
    map.insert("IB", 46);
    map.insert("IC", 16);
    map.insert("ID", 26);
    map.insert("IE", 41);
    map.insert("IF", 6);
    map.insert("IG", 10);
    map.insert("IH", 91);
    map.insert("II", 85);
    map.insert("IJ", 35);
    map.insert("IK", 9);
    map.insert("IL", 37);
    map.insert("IM", 74);
    map.insert("IN", 23);
    map.insert("IO", 84);
    map.insert("IP", 45);
    map.insert("IQ", 23);
    map.insert("IR", 36);
    map.insert("IS", 15);
    map.insert("IT", 72);
    map.insert("IU", 8);
    map.insert("IV", 93);
    map.insert("IW", 20);
    map.insert("IX", 9);
    map.insert("IY", 85);
    map.insert("IZ", 1);
    map.insert("JA", 6);
    map.insert("JB", 65);
    map.insert("JC", 75);
    map.insert("JD", 49);
    map.insert("JE", 62);
    map.insert("JF", 86);
    map.insert("JG", 35);
    map.insert("JH", 19);
    map.insert("JI", 25);
    map.insert("JJ", 6);
    map.insert("JK", 3);
    map.insert("JL", 49);
    map.insert("JM", 79);
    map.insert("JN", 5);
    map.insert("JO", 54);
    map.insert("JP", 90);
    map.insert("JQ", 77);
    map.insert("JR", 40);
    map.insert("JS", 32);
    map.insert("JT", 80);
    map.insert("JU", 38);
    map.insert("JV", 11);
    map.insert("JW", 55);
    map.insert("JX", 64);
    map.insert("JY", 49);
    map.insert("JZ", 3);
    map.insert("KA", 44);
    map.insert("KB", 80);
    map.insert("KC", 12);
    map.insert("KD", 64);
    map.insert("KE", 28);
    map.insert("KF", 14);
    map.insert("KG", 36);
    map.insert("KH", 80);
    map.insert("KI", 88);
    map.insert("KJ", 8);
    map.insert("KK", 49);
    map.insert("KL", 61);
    map.insert("KM", 88);
    map.insert("KN", 15);
    map.insert("KO", 69);
    map.insert("KP", 88);
    map.insert("KQ", 25);
    map.insert("KR", 6);
    map.insert("KS", 82);
    map.insert("KT", 67);
    map.insert("KU", 86);
    map.insert("KV", 78);
    map.insert("KW", 27);
    map.insert("KX", 76);
    map.insert("KY", 61);
    map.insert("KZ", 15);
    map.insert("LA", 41);
    map.insert("LB", 73);
    map.insert("LC", 6);
    map.insert("LD", 22);
    map.insert("LE", 56);
    map.insert("LF", 93);
    map.insert("LG", 77);
    map.insert("LH", 14);
    map.insert("LI", 42);
    map.insert("LJ", 81);
    map.insert("LK", 0);
    map.insert("LL", 42);
    map.insert("LM", 34);
    map.insert("LN", 64);
    map.insert("LO", 24);
    map.insert("LP", 79);
    map.insert("LQ", 58);
    map.insert("LR", 16);
    map.insert("LS", 57);
    map.insert("LT", 4);
    map.insert("LU", 9);
    map.insert("LV", 8);
    map.insert("LW", 13);
    map.insert("LX", 62);
    map.insert("LY", 82);
    map.insert("LZ", 88);
    map.insert("MA", 94);
    map.insert("MB", 79);
    map.insert("MC", 41);
    map.insert("MD", 47);
    map.insert("ME", 16);
    map.insert("MF", 75);
    map.insert("MG", 18);
    map.insert("MH", 23);
    map.insert("MI", 72);
    map.insert("MJ", 88);
    map.insert("MK", 11);
    map.insert("ML", 18);
    map.insert("MM", 6);
    map.insert("MN", 46);
    map.insert("MO", 22);
    map.insert("MP", 21);
    map.insert("MQ", 96);
    map.insert("MR", 7);
    map.insert("MS", 42);
    map.insert("MT", 26);
    map.insert("MU", 64);
    map.insert("MV", 35);
    map.insert("MW", 93);
    map.insert("MX", 78);
    map.insert("MY", 86);
    map.insert("MZ", 55);
    map.insert("NA", 69);
    map.insert("NB", 53);
    map.insert("NC", 18);
    map.insert("ND", 8);
    map.insert("NE", 81);
    map.insert("NF", 52);
    map.insert("NG", 29);
    map.insert("NH", 56);
    map.insert("NI", 7);
    map.insert("NJ", 44);
    map.insert("NK", 78);
    map.insert("NL", 8);
    map.insert("NM", 93);
    map.insert("NN", 51);
    map.insert("NO", 12);
    map.insert("NP", 27);
    map.insert("NQ", 58);
    map.insert("NR", 77);
    map.insert("NS", 52);
    map.insert("NT", 30);
    map.insert("NU", 4);
    map.insert("NV", 81);
    map.insert("NW", 17);
    map.insert("NX", 50);
    map.insert("NY", 63);
    map.insert("NZ", 84);
    map.insert("OA", 38);
    map.insert("OB", 36);
    map.insert("OC", 34);
    map.insert("OD", 50);
    map.insert("OE", 77);
    map.insert("OF", 68);
    map.insert("OG", 77);
    map.insert("OH", 71);
    map.insert("OI", 12);
    map.insert("OJ", 42);
    map.insert("OK", 67);
    map.insert("OL", 68);
    map.insert("OM", 80);
    map.insert("ON", 70);
    map.insert("OO", 65);
    map.insert("OP", 26);
    map.insert("OQ", 47);
    map.insert("OR", 96);
    map.insert("OS", 50);
    map.insert("OT", 81);
    map.insert("OU", 83);
    map.insert("OV", 83);
    map.insert("OW", 38);
    map.insert("OX", 56);
    map.insert("OY", 14);
    map.insert("OZ", 16);
    map.insert("PA", 99);
    map.insert("PB", 65);
    map.insert("PC", 49);
    map.insert("PD", 1);
    map.insert("PE", 82);
    map.insert("PF", 9);
    map.insert("PG", 74);
    map.insert("PH", 31);
    map.insert("PI", 48);
    map.insert("PJ", 84);
    map.insert("PK", 41);
    map.insert("PL", 33);
    map.insert("PM", 96);
    map.insert("PN", 90);
    map.insert("PO", 93);
    map.insert("PP", 21);
    map.insert("PQ", 30);
    map.insert("PR", 28);
    map.insert("PS", 95);
    map.insert("PT", 42);
    map.insert("PU", 91);
    map.insert("PV", 43);
    map.insert("PW", 100);
    map.insert("PX", 24);
    map.insert("PY", 82);
    map.insert("PZ", 33);
    map.insert("QA", 76);
    map.insert("QB", 56);
    map.insert("QC", 50);
    map.insert("QD", 20);
    map.insert("QE", 51);
    map.insert("QF", 53);
    map.insert("QG", 69);
    map.insert("QH", 89);
    map.insert("QI", 30);
    map.insert("QJ", 20);
    map.insert("QK", 45);
    map.insert("QL", 52);
    map.insert("QM", 83);
    map.insert("QN", 43);
    map.insert("QO", 57);
    map.insert("QP", 31);
    map.insert("QQ", 87);
    map.insert("QR", 14);
    map.insert("QS", 78);
    map.insert("QT", 77);
    map.insert("QU", 56);
    map.insert("QV", 55);
    map.insert("QW", 15);
    map.insert("QX", 90);
    map.insert("QY", 80);
    map.insert("QZ", 48);
    map.insert("RA", 18);
    map.insert("RB", 58);
    map.insert("RC", 41);
    map.insert("RD", 88);
    map.insert("RE", 2);
    map.insert("RF", 46);
    map.insert("RG", 56);
    map.insert("RH", 24);
    map.insert("RI", 90);
    map.insert("RJ", 21);
    map.insert("RK", 95);
    map.insert("RL", 51);
    map.insert("RM", 88);
    map.insert("RN", 49);
    map.insert("RO", 80);
    map.insert("RP", 21);
    map.insert("RQ", 48);
    map.insert("RR", 22);
    map.insert("RS", 72);
    map.insert("RT", 89);
    map.insert("RU", 70);
    map.insert("RV", 3);
    map.insert("RW", 96);
    map.insert("RX", 76);
    map.insert("RY", 42);
    map.insert("RZ", 53);
    map.insert("SA", 3);
    map.insert("SB", 0);
    map.insert("SC", 33);
    map.insert("SD", 0);
    map.insert("SE", 2);
    map.insert("SF", 14);
    map.insert("SG", 52);
    map.insert("SH", 100);
    map.insert("SI", 100);
    map.insert("SJ", 54);
    map.insert("SK", 76);
    map.insert("SL", 21);
    map.insert("SM", 97);
    map.insert("SN", 5);
    map.insert("SO", 42);
    map.insert("SP", 64);
    map.insert("SQ", 14);
    map.insert("SR", 89);
    map.insert("SS", 53);
    map.insert("ST", 64);
    map.insert("SU", 24);
    map.insert("SV", 77);
    map.insert("SW", 98);
    map.insert("SX", 2);
    map.insert("SY", 45);
    map.insert("SZ", 47);
    map.insert("TA", 100);
    map.insert("TB", 24);
    map.insert("TC", 27);
    map.insert("TD", 64);
    map.insert("TE", 36);
    map.insert("TF", 58);
    map.insert("TG", 45);
    map.insert("TH", 33);
    map.insert("TI", 6);
    map.insert("TJ", 89);
    map.insert("TK", 13);
    map.insert("TL", 56);
    map.insert("TM", 0);
    map.insert("TN", 93);
    map.insert("TO", 37);
    map.insert("TP", 34);
    map.insert("TQ", 99);
    map.insert("TR", 66);
    map.insert("TS", 42);
    map.insert("TT", 80);
    map.insert("TU", 40);
    map.insert("TV", 17);
    map.insert("TW", 72);
    map.insert("TX", 64);
    map.insert("TY", 86);
    map.insert("TZ", 26);
    map.insert("UA", 17);
    map.insert("UB", 72);
    map.insert("UC", 40);
    map.insert("UD", 28);
    map.insert("UE", 95);
    map.insert("UF", 49);
    map.insert("UG", 74);
    map.insert("UH", 77);
    map.insert("UI", 26);
    map.insert("UJ", 98);
    map.insert("UK", 85);
    map.insert("UL", 37);
    map.insert("UM", 87);
    map.insert("UN", 78);
    map.insert("UO", 54);
    map.insert("UP", 67);
    map.insert("UQ", 86);
    map.insert("UR", 84);
    map.insert("US", 79);
    map.insert("UT", 26);
    map.insert("UU", 22);
    map.insert("UV", 52);
    map.insert("UW", 89);
    map.insert("UX", 33);
    map.insert("UY", 32);
    map.insert("UZ", 67);
    map.insert("VA", 88);
    map.insert("VB", 22);
    map.insert("VC", 94);
    map.insert("VD", 59);
    map.insert("VE", 66);
    map.insert("VF", 51);
    map.insert("VG", 98);
    map.insert("VH", 40);
    map.insert("VI", 73);
    map.insert("VJ", 41);
    map.insert("VK", 8);
    map.insert("VL", 29);
    map.insert("VM", 51);
    map.insert("VN", 38);
    map.insert("VO", 95);
    map.insert("VP", 70);
    map.insert("VQ", 3);
    map.insert("VR", 94);
    map.insert("VS", 66);
    map.insert("VT", 13);
    map.insert("VU", 82);
    map.insert("VV", 98);
    map.insert("VW", 65);
    map.insert("VX", 4);
    map.insert("VY", 9);
    map.insert("VZ", 72);
    map.insert("WA", 54);
    map.insert("WB", 2);
    map.insert("WC", 30);
    map.insert("WD", 45);
    map.insert("WE", 77);
    map.insert("WF", 84);
    map.insert("WG", 24);
    map.insert("WH", 9);
    map.insert("WI", 4);
    map.insert("WJ", 55);
    map.insert("WK", 22);
    map.insert("WL", 34);
    map.insert("WM", 97);
    map.insert("WN", 50);
    map.insert("WO", 30);
    map.insert("WP", 84);
    map.insert("WQ", 46);
    map.insert("WR", 60);
    map.insert("WS", 12);
    map.insert("WT", 19);
    map.insert("WU", 47);
    map.insert("WV", 47);
    map.insert("WW", 43);
    map.insert("WX", 80);
    map.insert("WY", 99);
    map.insert("WZ", 86);
    map.insert("XA", 7);
    map.insert("XB", 81);
    map.insert("XC", 61);
    map.insert("XD", 85);
    map.insert("XE", 40);
    map.insert("XF", 30);
    map.insert("XG", 38);
    map.insert("XH", 66);
    map.insert("XI", 23);
    map.insert("XJ", 4);
    map.insert("XK", 3);
    map.insert("XL", 56);
    map.insert("XM", 59);
    map.insert("XN", 53);
    map.insert("XO", 37);
    map.insert("XP", 88);
    map.insert("XQ", 98);
    map.insert("XR", 36);
    map.insert("XS", 98);
    map.insert("XT", 12);
    map.insert("XU", 57);
    map.insert("XV", 36);
    map.insert("XW", 62);
    map.insert("XX", 77);
    map.insert("XY", 55);
    map.insert("XZ", 83);
    map.insert("YA", 19);
    map.insert("YB", 96);
    map.insert("YC", 6);
    map.insert("YD", 29);
    map.insert("YE", 98);
    map.insert("YF", 99);
    map.insert("YG", 10);
    map.insert("YH", 69);
    map.insert("YI", 65);
    map.insert("YJ", 35);
    map.insert("YK", 16);
    map.insert("YL", 51);
    map.insert("YM", 89);
    map.insert("YN", 46);
    map.insert("YO", 39);
    map.insert("YP", 44);
    map.insert("YQ", 86);
    map.insert("YR", 32);
    map.insert("YS", 80);
    map.insert("YT", 20);
    map.insert("YU", 51);
    map.insert("YV", 6);
    map.insert("YW", 27);
    map.insert("YX", 93);
    map.insert("YY", 4);
    map.insert("YZ", 95);
    map.insert("ZA", 46);
    map.insert("ZB", 99);
    map.insert("ZC", 25);
    map.insert("ZD", 97);
    map.insert("ZE", 75);
    map.insert("ZF", 1);
    map.insert("ZG", 54);
    map.insert("ZH", 84);
    map.insert("ZI", 63);
    map.insert("ZJ", 81);
    map.insert("ZK", 86);
    map.insert("ZL", 80);
    map.insert("ZM", 38);
    map.insert("ZN", 92);
    map.insert("ZO", 93);
    map.insert("ZP", 68);
    map.insert("ZQ", 68);
    map.insert("ZR", 49);
    map.insert("ZS", 33);
    map.insert("ZT", 84);
    map.insert("ZU", 21);
    map.insert("ZV", 8);
    map.insert("ZW", 98);
    map.insert("ZX", 17);
    map.insert("ZY", 58);
    map.insert("ZZ", 25);

    let (encode_book, decode_book) = ec::from_iter(map.iter());

    let c: Vec<&&str> = map.keys().collect();
    let example = black_box(c);
    b.iter(|| {
        let mut buffer: Vec<&String> = Vec::new();
        for symbol in &example {
            ec::encode_symbol_from_buffer(&encode_book, *symbol, &mut buffer);
            //buffer += ec::encode_symbol(&encode_book, symbol).unwrap();
        }

        //assert!(example
        //    .iter()
        //    .zip(ec::decode_str(&decode_book, &buffer))
        //    .all(|(l, r)| *l == r));
    });
}

benchmark_group!(benches, bench_encode_decode);

benchmark_main!(benches);
