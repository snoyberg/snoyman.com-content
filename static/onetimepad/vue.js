// http://stackoverflow.com/questions/10319415/unicode-to-binary
var UTF_BITS = 8;

function padLeftTo(string, padChar, numChars) {
    return (new Array(numChars-string.length+1)).join(padChar) + string;
}

function unicodeToBinary(char) {
    return char.split('').map(function(codepoint) {
        return padLeftTo(codepoint.charCodeAt(0).toString(2), 0, UTF_BITS);
    }).join('').split('').map(function(char){return parseInt(char)});
    //         ^^^^( ignore this part if you just want a string )^^^^
}


function binaryToUnicode(binaryList) {
    var codepointsAsNumbers = [];
    while( binaryList.length>0 ){
        var codepointBits = binaryList.slice(0,UTF_BITS);
        binaryList = binaryList.slice(UTF_BITS);
        codepointsAsNumbers.push( parseInt(codepointBits.join(''),2) );
    }
    return String.fromCharCode.apply(this,codepointsAsNumbers);
}

function newPad(len) {
    var arr = new Uint8Array(len)
    window.crypto.getRandomValues(arr)
    return arr.map(function(x) { return x % 2 })
}

function xor(arr1, arr2) {
    return arr1.map(function(x, i) {
        var y = arr2[i];
        return x == y ? 0 : 1;
    })
}

new Vue({
    el: "#encrypt",
    data: {
        cleartext: "Enter your message"
    },
    computed: {
        binary: function() {
            var cleartext = unicodeToBinary(this.cleartext),
                pad = newPad(cleartext.length),
                ciphertext = xor(cleartext, pad)
            return {
                cleartext: cleartext.join(""),
                pad: pad.join(""),
                ciphertext: ciphertext.join("")
            }
        }
    }
});


new Vue({
    el: "#decrypt",
    data: {
        pad: "1000101101100111111100101011001000101011011110010111111010111110010011000100100011100011",
        ciphertext: "1100001100000010100111101101111001000100010110010010100111010001001111100010010010000111"
    },
    computed: {
        cleartext: function() {
            var binary = xor(this.pad.split(""), this.ciphertext.split(""));
            return {
                binary: binary.join(""),
                text: binaryToUnicode(binary)
            }
        }
    }
});
