//Q1
export function all(promises: Array<Promise<any>>): Promise<Array<any>> {
  return new Promise((resolve, reject) => {
    
    const results: any[] = [];
    let remaining = promises.length;

    if (remaining === 0) {
      return resolve(results);
    }

    promises.forEach((p, idx) => {
      Promise.resolve(p)
        .then(value => {
          results[idx] = value;
          remaining--;

          if (remaining === 0) {
            resolve(results);
          }
        })
        .catch(err => {
          reject(err);
        });
    });
  });
}
  
// Q2
export function* Fib1() {
  let a = 1, b = 1;
  while (true) {
    yield a;         
    [a, b] = [b, a + b];
  }
}


export function* Fib2() {
  const sqrt5 = Math.sqrt(5);
  const phi = (1 + sqrt5) / 2;
  const psi = (1 - sqrt5) / 2;
  let n = 1;
  while (true) {
    const fn = Math.round((phi ** n - psi ** n) / sqrt5);
    yield fn;
    n++;
  }
}
