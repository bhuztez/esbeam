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

function subview(view, offset, length) {
    return new DataView(view.buffer, view.byteOffset + offset, length);
}

function load_atoms(view) {
    let decoder = new TextDecoder("latin1", {fatal: true});

    let num = view.getUint32(0);
    let atoms = new Array(num);

    let offset = 4;

    for(let i=0; i<num; i++) {
        let size = view.getUint8(offset);
        let s = decoder.decode(subview(view, offset+1, size));
        atoms[i] = Symbol.for(s);
        offset += size + 1;
    }

    return atoms;
}

function load_beam(buffer) {
    let view = new DataView(buffer);
    if (view.getUint32(0) !== 0x464f5231) return null;
    if (view.getUint32(4) + 8 !== view.byteLength) return null;
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
        return new DataView(buffer, offset + 8, length);
    }

    let atoms = load_atoms(get_chunk(0x41746f6d));
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
