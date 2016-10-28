import {Integer} from "beam/integer.js";


function compare_digits(a, b) {
    if ((a === null) && (b === null))
        return true;

    if ((a !== null) && (b === null))
        return false;

    if ((a === null) && (b !== null))
        return false;

    b = Uint32Array.from(b);

    if (a.length !== b.length)
        return false;

    let length = a.length;

    for(let i=0; i<length; i++)
        if (a[i] !== b[i])
            return false;

    return true;
}


function test_from_bytes(utils, signed, data, sign, digits) {
    let buffer = Uint8Array.from(data).buffer;
    let view = new DataView(buffer);

    let i = Integer.from_bytes(view, signed);
    utils.assert(i.sign === sign);
    utils.assert(compare_digits(i.digits, digits));
}

export function test_from_1_byte(utils) {
    test_from_bytes(utils, false, [255], 255, null);
    test_from_bytes(utils, true, [128], -128, null);
}

export function test_from_2_byte(utils) {
    test_from_bytes(utils, false, [255,255], 65535, null);
    test_from_bytes(utils, true, [128,0], -32768, null);
}

export function test_from_3_byte(utils) {
    test_from_bytes(utils, false, [255,255,255], 16777215, null);
    test_from_bytes(utils, true, [128,0,0], -8388608, null);
}

export function test_from_4_byte(utils) {
    test_from_bytes(utils, false, [255,255,255,255], 4294967295, null);
    test_from_bytes(utils, true, [128,0,0,0], -2147483648, null);
}

export function test_from_5_byte(utils) {
    test_from_bytes(utils, false, [255,255,255,255,255], 1, [4294967295,255]);
    test_from_bytes(utils, true, [128,0,0,0,0], -1, [0,128]);
    test_from_bytes(utils, true, [0,255,255,255,255], 4294967295, null);
    test_from_bytes(utils, true, [255,0,0,0,1], -4294967295, null);
}

export function test_from_6_byte(utils) {
    test_from_bytes(utils, false, [255,255,255,255,255,255], 1, [4294967295,65535]);
    test_from_bytes(utils, true, [128,0,0,0,0,0], -1, [0,32768]);
}

export function test_from_7_byte(utils) {
    test_from_bytes(utils, false, [255,255,255,255,255,255,255], 1, [4294967295,16777215]);
    test_from_bytes(utils, true, [128,0,0,0,0,0,0], -1, [0,8388608]);
}

export function test_from_8_byte(utils) {
    test_from_bytes(utils, false, [255,255,255,255,255,255,255,255], 1, [4294967295,4294967295])
    test_from_bytes(utils, true, [128,0,0,0,0,0,0,0], -1, [0,2147483648]);
}
