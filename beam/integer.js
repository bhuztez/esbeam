export class Integer {
    constructor(sign, digits) {
        this.sign = sign;
        this.digits = digits;
    }

    static from_bytes(bytes, signed) {
        let sign = 1;
        if (signed === true)
            if (bytes.getInt8(0) < 0)
                sign = -1;

        let length = bytes.byteLength;
        let n = Math.floor((length-1)/4);
        let remain = length - n*4;

        let v = 0;
        for(let i=0; i<remain; i++) {
            v <<= 8;
            let d = bytes.getUint8(i);
            if (sign < 0)
                d = (~d) & 0xFF;
            v |= d;
        }
        v >>>= 0;

        let array = null;

        if (n === 0) {
            sign = (sign<0)?-(v+1):v;
        } else {
            array = new Uint32Array(n+1);
            let carry = 1;

            for(let i=0; i<n; i++) {
                let d = bytes.getUint32(length - 4 - 4*i, false);
                if (sign < 0) {
                    d = (~d) >>> 0;
                    d += carry;
                    carry = (d>0xFFFFFFFF)?1:0;
                    d >>>= 0;
                }
                array[i] = d;
            }

            array[n] = (sign<0)?(v+carry):v;

            for(;(n>0);n--)
                if(array[n] !== 0)
                    break;

            let exact = 4*(n+1);

            if (n === 0) {
                sign *= array[0];
                array = null;
            } else if (array.byteLength > exact) {
                array = new Uint32Array(ArrayBuffer.transfer(array.buffer, exact));
            }
        }
        return new Integer(sign, array);
    }
}
