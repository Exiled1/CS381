const callBy = "value";

let v = {
    z,
    y:5,
    f(x){
        x:x+1
        y:x-4
        x:x+1
        return x;
    },
    z:f(y)+y,
};
const eva = (val) => {
  if (typeof val === "function") {
    return val();
  } else {
    return val;
  }
};
const newVar = (name) => {
  return {
    val: null,
    id: name,
    cached: false,
    get get() {
      if (callBy == "value") {
        return this.val;
      }
      if (callBy == "need") {
        if (this.cached) {
          return v[this.id];
        }
        const ret = eva(this.val);
        v[this.id] = ret;
        this.cached = true;
        return ret;
      }
      if (callBy == "name") {
        const ret = eva(this.val);
        v[this.id] = ret;
        return ret;
      }
      return eva(this.val);
    },
    set set(val) {
      console.log(v, [this.id, this.val]);
      this.cached = false;
      if (callBy == "name") {
        this.val = val;
      }
      if (callBy == "need") {
        this.val = eva(val);
      }
      if (callBy == "value") {
        this.val = eva(val);
      }
      v[this.id] = this.val;
    },
  };
};
const varFrom = (name, val) => {
  const va = newVar(name);
  va.set = val;
  return va;
};
const deleteEntry = (obj, name) =>
  Object.fromEntries(Object.entries(obj).filter(([k, v]) => k !== name));
const objectIntersection = (object1, object2) =>
  Object.fromEntries(Object.entries(object1).filter(([k, v]) => k in object2));
const pop = (ret, boundVars) =>
  [
    ret,
    boundVars.map((va) => {
      v = objectIntersection(v, deleteEntry(v, va.id));
    }),
  ].shift();

const [y, z] = ["y", "z"].map((n) => newVar(n));
y.set = 6;
const f = function (a) {
  a = varFrom("a", a);
  y.set = a.get + 5;
  return pop(y.get + a.get, [a]);
};
const g = function (x) {
  x = varFrom("x", x);
  y.set = f(() => x.get + 1) + 1;
  z.set = f(() => x.get - y.get + 3);
  return pop(z.get + 2, [x]);
};
z.set = g(() => y.get * 2);
console.log("final: ", v);
