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
	// @TODO
  return undefined;
}


export function* Fib2() {
	// @TODO
  return undefined;
}
