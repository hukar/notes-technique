let hello = "hello kiki" //?+

hello.__proto__ //?+
hello.__proto__.__proto__ //?
hello.__proto__.__proto__.__proto__ //?

function Voiture() {}
 Voiture.prototype.accelerer = function() {
        console.log('vrouuum')
}

Voiture.prototype.freiner = function() {
        console.log('huuuuuuu !!')
}

Voiture.prototype //?



niu = Object.setPrototypeOf(hello, Voiture)
niu.__proto__.__proto__.__proto__ //?
Voiture.__proto__.__proto__.__proto__ //?
niu.freiner() //?+ 

