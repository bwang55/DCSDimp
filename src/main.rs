use std::alloc::System;
use rand::distributions::Distribution;
use rand::distributions::WeightedIndex;
use rand::prelude::ThreadRng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::ops::DerefMut;

struct Sampler {
    random: RefCell<ThreadRng>,
    distribution: WeightedIndex<f64>,
    source: Vec<u64>,
}


impl Sampler {
    fn new<T: Iterator<Item = (u64, f64)>>(t: T) -> Sampler {
        let r = RefCell::new(rand::thread_rng());
        let vector: Vec<(u64, f64)> = t.into_iter().collect(); //Guarantees our index ordering.
        let distribution = WeightedIndex::new(vector.iter().map(|(_, weight)| *weight)).unwrap();
        let source = vector.into_iter().map(|(item, _)| item).collect();

        Sampler {
            random: r,
            distribution,
            source,
        }
    }

    fn sample(&self) -> u64 {
        let index = self
            .distribution
            .sample(self.random.borrow_mut().deref_mut());
        self.source[index]
    }
}

struct Simulator {
    size: u64,
    tracker: HashMap<u64, u64>,
    step: u64,
}

impl Simulator {
    fn init() -> Simulator {
        Simulator {
            size: 0,
            tracker: HashMap::new(),
            step: 0,
        }
    }

    fn add_tenancy(&mut self, tenancy: u64) {
        self.update();
        self.size += 1;
        let target = tenancy + self.step;
        let expirations_at_step = self.tracker.get(&target).copied().unwrap_or(0);
        self.tracker.insert(target, expirations_at_step + 1);
    }

    fn update(&mut self) {
        self.step += 1;
        self.size -= self.tracker.remove(&self.step).unwrap_or(0);
    }

    fn get_excess(&self, fixed: u64) -> u64 {
        if self.size <= fixed {
            0
        } else {
            self.size - fixed
        }
    }

    fn _get_size(&self) -> u64 {
        self.size
    }
}

fn get_distribution(inp:&Vec<u64>) -> Vec<f64>{
    let sum = get_sum(inp);
    let mut index:usize = 0;
    let leng = inp.len();
    let mut result:Vec<f64> = vec![0.0; leng];
    while index < leng {
        result[index] = (inp[index] as f64) / (sum as f64);
        index += 1;
    }
    return result;
}


fn get_distribution_difference(a:&Vec<u64>, b:&Vec<u64>) -> f64{
    let mut index = 0;
    let leng = a.len();
    let a_dist = get_distribution(a);
    let b_dist = get_distribution(b);
    // for i in &a_dist{
    //     println!("{}------------as", i)
    // }


    let mut sum:f64 = 0.0;
    while index < leng -1 {
        let mut diff:f64 = a_dist[index] - b_dist[index];
        sum += diff.abs();
        index += 1;
    }
    return sum;
}


fn caching(ten_dist: Sampler, cache_size: u64, delta: f64, length:usize) -> Vec<u64> {//HashMap<u64, u64> {
    let mut cache = Simulator::init();
    let mut trace_len: u64 = 0;
    let mut samples_to_issue: u64 = 1024;
    let mut prev_output: Vec<u64> = vec![0; length + 1];
    let mut total_overalloc: u64 = 0;
    let mut dcsd_observed = vec![0; length + 1];
    // This code is currently reporting the scalar value of overallocations
    // Instead, we want to calculate the DCS distribution
    // We need to initialize some vector dcsd_observed, of length T_MAX
    // At each time step, intead of incrementing total_overalloc, instead we increment
    // dcsd_observed[cache.size]

    //Convergence should be decided based on total variation distance (L1 Norm) being less than
    //delta.
    let mut time = 0;
    loop {
        if time >= 0{
            break
        }
        // for _ in 0..samples_to_issue -1 {
            let tenancy = ten_dist.sample();
            cache.add_tenancy(tenancy);
        // }
        time += 1;
        }


    let mut cycles = 0;
    loop {
        if cycles > 100000{
            return dcsd_observed.clone();
        }
        for _ in 0..samples_to_issue -1 {
            trace_len += 1;
            let tenancy = ten_dist.sample();
            cache.add_tenancy(tenancy);
            total_overalloc += cache.get_excess(cache_size);
            dcsd_observed[cache.size as usize] += 1;
        }

        // if get_distribution_difference(&prev_output, &dcsd_observed) < delta//((total_overalloc as f64) / (trace_len as f64) - prev_output.unwrap()) < delta
        // {
        //
        //     println!("{} this is trace_len", trace_len);
        //     return dcsd_observed.clone();
        //
        // }
        prev_output = dcsd_observed.clone();//Some((total_overalloc as f64) / (trace_len as f64));
        // samples_to_issue *= 2;
        cycles += 1;
    }
}


fn copy_vector(input:&Vec<u64>) -> Vec<u64>{
    let mut output:Vec<u64> = vec![];
    for i in input{
        output.push(*i);
    }
    return output;
}


fn get_sum(input:&Vec<u64>) -> u128{
    let mut sum:u128 = 0;
    let mut index:usize = 0;
    for k in input{
        sum += *k as u128;
        if index == input.len(){
            break;
        }
        index += 1;
    }
    if sum == 0{
        return 1;
    }
    return sum;
}


fn get_overalloc_area(input:Vec<u64>, fcs:u64, length:usize) -> f64{

    let mut prba:Vec<f64> = vec![0.0;length];
    let mut index:usize = 0;

    let sum = get_sum(&input);

    index = 0;
    for key in &input{
        prba[index] = (*key as f64) / sum as f64;
        if index == prba.len(){
            break;
        }
        index += 1;
    }

    index = 0;
    let mut result = 0.0;


    for key in &input{
        if input[index] as isize - fcs as isize > 0{
            result += (input[index] - fcs) as f64 * (prba[index]);
        }
        if index == input.len(){
            break;
        }
        index += 1;
    }

    return result;
}


fn input_to_hashmap() -> (HashMap<u64, f64>, usize) {
    let mut rdr = csv::ReaderBuilder::new()
        .from_reader(io::stdin());
    let mut _result:HashMap<u64, f64> = HashMap::new();
    let mut largest = 0;
    for result in rdr.records() {
        let record = result.unwrap();
        if record.get(0).unwrap().parse::<usize>().unwrap() > largest{
            largest = record.get(0).unwrap().parse().unwrap();
        }
        _result.insert(record.get(0).unwrap().parse().unwrap(), record.get(1).unwrap().parse().unwrap());
    }
    return (_result, largest);
}


fn write(output: Vec<u64>){
    let sum = get_sum(&output);
    println!("The Sum is {}======================",sum);
    let mut wtr = csv::Writer::from_writer(io::stdout());
    let mut index:usize = 0;
    // keys.sort_unstable();
    wtr.write_record(&["DCS", "probability"]).expect("cannot write");
    for key in output{
        // wtr.write_record(&[index.to_string(), ((key as f64) .to_string())]);
        wtr.write_record(&[index.to_string(), ((key as f64) / sum as f64).to_string()]).expect("cannot write");
        index += 1;
    }
}


fn main() {

    let test = input_to_hashmap();
    // let (over_alloc, trace_len) = caching(Sampler::new(test.into_iter()), 10, 0.05);
    let test_1 = caching(Sampler::new(test.0.into_iter()), 10, 0.005, test.1);
    write(test_1);

}
