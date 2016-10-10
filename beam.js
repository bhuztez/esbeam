function asyncf(f){
    return function(...args){
        let it = f.apply(this, args);

        function next(p){
            if(!(p.done)){
                p.value.then(
                    (result => next(it.next(result))),
                    (exception => it.throw(exception)));
            }
        }
        next(it.next());
    };
}

let NIL = Symbol("[]");

function List(head, tail) {
    this.head = head;
    this.tail = tail;
};

function binary_to_list(view) {
    let len = view.byteLength;
    let list = NIL;
    for(let i=len-1; i>=0; i--) {
        list = new List(view.getUint8(i), list);
    }
    return list;
}

function Buffer(view, offset) {
    this.view = view;
    this.offset = offset || 0;
}

Buffer.prototype.end = function() {
    return (this.view.byteOffset + this.offset) >= this.view.buffer.byteLength;
}

Buffer.prototype.seek = function(n) {
    this.offset += n;
}

Buffer.prototype.readUint8 = function(littleEndian) {
    let i = this.view.getUint8(this.offset, littleEndian);
    this.offset += 1;
    return i;
}

Buffer.prototype.readUint16 = function(littleEndian) {
    let i = this.view.getUint16(this.offset, littleEndian);
    this.offset += 2;
    return i;
}

Buffer.prototype.readUint32 = function(littleEndian) {
    let i = this.view.getUint32(this.offset, littleEndian);
    this.offset += 4;
    return i;
}

Buffer.prototype.readInt8 = function(littleEndian) {
    let i = this.view.getInt8(this.offset, littleEndian);
    this.offset += 1;
    return i;
}

Buffer.prototype.readInt16 = function(littleEndian) {
    let i = this.view.getInt16(this.offset, littleEndian);
    this.offset += 2;
    return i;
}

Buffer.prototype.readInt32 = function(littleEndian) {
    let i = this.view.getInt32(this.offset, littleEndian);
    this.offset += 4;
    return i;
}

Buffer.prototype.readFloat32 = function(littleEndian) {
    let i = this.view.getFloat32(this.offset, littleEndian);
    this.offset += 4;
    return i;
}

Buffer.prototype.readFloat64 = function(littleEndian) {
    let i = this.view.getFloat64(this.offset, littleEndian);
    this.offset += 8;
    return i;
}

Buffer.prototype.readDataView = function(n) {
    let offset = this.view.byteOffset + this.offset;
    let length = n || (this.view.buffer.byteLength - offset);
    let view = new DataView(this.view.buffer, offset, length);
    this.offset += length;
    return view;
}

Buffer.prototype.readUint8Array = function(n) {
    let offset = this.view.byteOffset + this.offset;
    let length = n || (this.view.buffer.byteLength - offset);
    let array = new Uint8Array(this.view.buffer, offset, length);
    this.offset += length;
    return array;
}


let latin1_decoder = new TextDecoder("latin1", {fatal: true});

function load_atoms(buffer) {
    let num = buffer.readUint32();
    let atoms = new Array(num);

    for(let i=0; i<num; i++) {
        let size = buffer.readUint8();
        let s = latin1_decoder.decode(buffer.readDataView(size));
        atoms[i] = Symbol.for(s);
    }

    return atoms;
}

function load_imports(view) {
    let num = view.getUint32(0);
    let imports = new Array(num);

    for(let offset=4; offset<4+num*12; offset+=12) {
        let M = view.getUint32(offset);
        let F = view.getUint32(offset+4);
        let A = view.getUint32(offset+8);
    }
}


function decode_term(buffer) {
    let tag = buffer.readUint8();

    switch(tag) {
    case 97:
        return buffer.readUint8();
    case 98:
        return buffer.readInt32(true);
    case 107: {
        let len = buffer.readUint16();
        let chars = binary_to_list(buffer.readDataView(len));
        return chars;
    }
    case 109: {
        let len = buffer.readUint32();
        return buffer.readDataView(len);
    }
    default:
        console.log("unknown tag", tag);
        return null;
    }
}


function load_literals(buffer) {
    let size = buffer.readUint32(0);
    let array = buffer.readUint8Array(buffer.byteLength - 4);
    let uncompressed = new Buffer(new DataView(pako.inflate(array).buffer));

    if(uncompressed.view.buffer.byteLength !== size) {
        return null;
    }

    let num = uncompressed.readUint32();
    let literals = new Array(num);

    for(let i=0; i<num; i++) {
        let size = uncompressed.readUint32();
        let data = new Buffer(uncompressed.readDataView(size));

        if (data.readUint8() !== 131) {
            return null;
        }

        let term = decode_term(data);

        if (term === null) {
            return null;
        }

        literals[i] = term;
    }

    return literals;
}


function Line(fname, line) {
    this.fname = fname;
    this.line = line;
}

function decode_line(buffer) {
    // n>0
    // -n means {a,n}
    // n means {i,n}

    let byte = buffer.readUint8();
    let tag = (byte & 7);
    let n = 0;
    if ((byte & 0x08) === 0) {
        n = byte >> 4;
    } else if ((byte & 0x10) === 0) {
        n = (byte << 3) | buffer.readUint8();
    } else {
        let len = (byte >> 5) + 2;
        if (len > 4) { // unsupported
            return null;
        }

        n = (tag === 1)?buffer.readInt32():buffer.readUint32();
    }

    if (tag === 2)
        return -n;

    if (tag !== 1)
        return null;

    if (n < 0)
        return null;

    return n;
}


function load_lines(buffer) {
    let ver = buffer.readUint32();
    let bits = buffer.readUint32();
    let num_insts = buffer.readUint32();
    let num_lines = buffer.readUint32();
    let num_fnames = buffer.readUint32();

    let lines = new Array(num_lines);
    let fnames = new Array(num_fnames);
    let file = null;

    for(let i=0; i<num_lines; i++) {
        let l = decode_line(buffer);
        if (l === null)
            return null;

        if (l < 0) {
            file = -l;
            l = decode_line(buffer);
        }

        lines[i] = new Line(file, l);
    }

    for(let i=0; i<num_fnames; i++) {
        let size = buffer.readUint16();
        fnames[i] = latin1_decoder.decode(buffer.readDataView(size));
    }

    for(let i=0; i<num_lines; i++) {
        let f = lines[i].fname;
        if (f)
            lines[i].fname = fnames[f-1];
    }

    return lines;
}


let arity_table = [
    0,
    1, // 1: label/1
    3, // 2: func_info/3
    0, // 3: int_code_end/0
    2, // 4: call/2
    3, // 5: call_last/3
    2, // 6: call_only/2
    2, // 7: call_ext/2
    3, // 8: call_ext_last/3
    2, // 9: bif0/2
    4, // 10: bif1/4
    5, // 11: bif2/5
    2, // 12: allocate/2
    3, // 13: allocate_heap/3
    2, // 14: allocate_zero/2
    3, // 15: allocate_heap_zero/3
    2, // 16: test_heap/2
    1, // 17: init/1
    1, // 18: deallocate/1
    0, // 19: return/0
    0, // 20: send/0
    0, // 21: remove_message/0
    0, // 22: timeout/0
    2, // 23: loop_rec/2
    1, // 24: loop_rec_end/1
    1, // 25: wait/1
    2, // 26: wait_timeout/2
    4, // 27: -m_plus/4
    4, // 28: -m_minus/4
    4, // 29: -m_times/4
    4, // 30: -m_div/4
    4, // 31: -int_div/4
    4, // 32: -int_rem/4
    4, // 33: -int_band/4
    4, // 34: -int_bor/4
    4, // 35: -int_bxor/4
    4, // 36: -int_bsl/4
    4, // 37: -int_bsr/4
    3, // 38: -int_bnot/3
    3, // 39: is_lt/3
    3, // 40: is_ge/3
    3, // 41: is_eq/3
    3, // 42: is_ne/3
    3, // 43: is_eq_exact/3
    3, // 44: is_ne_exact/3
    2, // 45: is_integer/2
    2, // 46: is_float/2
    2, // 47: is_number/2
    2, // 48: is_atom/2
    2, // 49: is_pid/2
    2, // 50: is_reference/2
    2, // 51: is_port/2
    2, // 52: is_nil/2
    2, // 53: is_binary/2
    2, // 54: -is_constant/2
    2, // 55: is_list/2
    2, // 56: is_nonempty_list/2
    2, // 57: is_tuple/2
    3, // 58: test_arity/3
    3, // 59: select_val/3
    3, // 60: select_tuple_arity/3
    1, // 61: jump/1
    2, // 62: catch/2
    1, // 63: catch_end/1
    2, // 64: move/2
    3, // 65: get_list/3
    3, // 66: get_tuple_element/3
    3, // 67: set_tuple_element/3
    3, // 68: -put_string/3
    3, // 69: put_list/3
    2, // 70: put_tuple/2
    1, // 71: put/1
    1, // 72: badmatch/1
    0, // 73: if_end/0
    1, // 74: case_end/1
    1, // 75: call_fun/1
    3, // 76: -make_fun/3
    2, // 77: is_function/2
    2, // 78: call_ext_only/2
    2, // 79: -bs_start_match/2
    5, // 80: -bs_get_integer/5
    5, // 81: -bs_get_float/5
    5, // 82: -bs_get_binary/5
    4, // 83: -bs_skip_bits/4
    2, // 84: -bs_test_tail/2
    1, // 85: -bs_save/1
    1, // 86: -bs_restore/1
    2, // 87: -bs_init/2
    2, // 88: -bs_final/2
    5, // 89: bs_put_integer/5
    5, // 90: bs_put_binary/5
    5, // 91: bs_put_float/5
    2, // 92: bs_put_string/2
    1, // 93: -bs_need_buf/1
    0, // 94: fclearerror/0
    1, // 95: fcheckerror/1
    2, // 96: fmove/2
    2, // 97: fconv/2
    4, // 98: fadd/4
    4, // 99: fsub/4
    4, // 100: fmul/4
    4, // 101: fdiv/4
    3, // 102: fnegate/3
    1, // 103: make_fun2/1
    2, // 104: try/2
    1, // 105: try_end/1
    1, // 106: try_case/1
    1, // 107: try_case_end/1
    2, // 108: raise/2
    6, // 109: bs_init2/6
    3, // 110: -bs_bits_to_bytes/3
    5, // 111: bs_add/5
    1, // 112: apply/1
    2, // 113: apply_last/2
    2, // 114: is_boolean/2
    3, // 115: is_function2/3
    5, // 116: bs_start_match2/5
    7, // 117: bs_get_integer2/7
    7, // 118: bs_get_float2/7
    7, // 119: bs_get_binary2/7
    5, // 120: bs_skip_bits2/5
    3, // 121: bs_test_tail2/3
    2, // 122: bs_save2/2
    2, // 123: bs_restore2/2
    5, // 124: gc_bif1/5
    6, // 125: gc_bif2/6
    2, // 126: -bs_final2/2
    2, // 127: -bs_bits_to_bytes2/2
    2, // 128: -put_literal/2
    2, // 129: is_bitstr/2
    1, // 130: bs_context_to_binary/1
    3, // 131: bs_test_unit/3
    4, // 132: bs_match_string/4
    0, // 133: bs_init_writable/0
    8, // 134: bs_append/8
    6, // 135: bs_private_append/6
    2, // 136: trim/2
    6, // 137: bs_init_bits/6
    5, // 138: bs_get_utf8/5
    4, // 139: bs_skip_utf8/4
    5, // 140: bs_get_utf16/5
    4, // 141: bs_skip_utf16/4
    5, // 142: bs_get_utf32/5
    4, // 143: bs_skip_utf32/4
    3, // 144: bs_utf8_size/3
    3, // 145: bs_put_utf8/3
    3, // 146: bs_utf16_size/3
    3, // 147: bs_put_utf16/3
    3, // 148: bs_put_utf32/3
    0, // 149: on_load/0
    1, // 150: recv_mark/1
    1, // 151: recv_set/1
    7, // 152: gc_bif3/7
    1, // 153: line/1
    5, // 154: put_map_assoc/5
    5, // 155: put_map_exact/5
    2, // 156: is_map/2
    3, // 157: has_map_fields/3
    3, // 158: get_map_elements/3
];


function Tagged(tag, value) {
    this.tag = tag;
    this.value = value;
}


function decode_arg(buffer) {
    let byte = buffer.readUint();
    let tag = byte & 0x7;

    if (tag === 7) {
        let n = byte >> 3;
    }
}


function decode_args(buffer, n) {
    let args = new Array(arity);

    for(let i=0; i<arity; i++) {
        let arg = decode_arg(buffer);
        if (arg === null)
            return null;
        args[i] = arg;
    }
    return args;
}

function load_code(buffer) {
    let offset = buffer.readUint32();
    let version = buffer.readUint32();
    let opcode_max = buffer.readUint32();
    let num_labels = buffer.readUint32();
    let num_lines = buffer.readUint32();


    while(!(buffer.end())){
        let op = buffer.readUint8();
        let arity = arity_table[op];
        let args = decode_args(buffer, arity);
    }
}


function load_beam(buffer) {
    let view = new DataView(buffer);
    // FOR1
    if (view.getUint32(0) !== 0x464f5231) return null;
    if (view.getUint32(4) + 8 !== view.byteLength) return null;
    // BEAM
    if (view.getUint32(8) !== 0x4245414d) return null;

    let offsets = new Map();

    for(let offset=12; offset<view.byteLength; ){
        let name = view.getUint32(offset);
        offsets.set(name, offset);
        let length = view.getUint32(offset+4);
        let rem = length % 4;
        let size = rem?length+4-rem:length;
        offset += 8 + size;
    }

    function get_chunk(name) {
        let offset = offsets.get(name);
        let length = view.getUint32(offset + 4);
        return new Buffer(new DataView(buffer, offset + 8, length));
    }

    // Atom
    let atoms = load_atoms(get_chunk(0x41746f6d));
    // LitT
    let literals = load_literals(get_chunk(0x4c697454));
    // Line
    let lines = load_lines(get_chunk(0x4c696e65));
    // Code
    let code = load_code(get_chunk(0x436f6465));

}

let host = "http://127.0.0.1:8000/"

window.addEventListener(
    'load',
    function () {
        asyncf(function*(){
            let response = yield fetch(host+"otp_ring0.beam");
            let buffer = yield response.arrayBuffer();
            load_beam(buffer);

        })();
    });
