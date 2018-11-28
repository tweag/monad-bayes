import matplotlib.pyplot as plt
import pandas as pd
import re

benchmark_pattern = "(?P<system>(MonadBayes|Anglican|WebPPL))_(?P<model>(LR|HMM|LDA))(?P<length>[0-9]+)_(?P<alg>(SMC(?P<smcparam>[0-9]+$)|MH(?P<mhparam>[0-9]+$)|RMSMC(?P<rmsmcparam>[0-9]+-[0-9]+$)))"
benchmark_reg = re.compile(benchmark_pattern)
rmsmc_pattern = "(?P<particles>[0-9]+)-(?P<steps>[0-9]+)"
rmsmc_reg = re.compile(rmsmc_pattern)


def unpack_name (benchmark_name):
    m = benchmark_reg.match(benchmark_name)
    if m is None:
        return None
    def lookup (property_name):
        return m.expand("\g<" + property_name + ">")
    system = lookup("system")
    model = lookup("model")
    length = int(lookup("length"))
    alg = lookup("alg")
    if alg[:3] == "SMC":
        alg_name = "SMC"
        particles = int(lookup("smcparam"))
        steps = 0
    elif alg[:2] == "MH":
        alg_name = "MH"
        particles = 0
        steps = int(lookup("mhparam"))
    elif alg[:5] == "RMSMC":
        alg_name = "RMSMC"
        t = rmsmc_reg.match(lookup("rmsmcparam"))
        particles = int(t.expand("\g<particles>"))
        steps = int(t.expand("\g<steps>"))
    else:
        raise ValueError("Unrecognized algorithm: " + alg)
    return system, model, length, alg_name, particles, steps

def unpack_names (series):
    x = list(filter(lambda y: y is not None, [unpack_name(name) for name in series]))
    systems = [y[0] for y in x]
    models = [y[1] for y in x]
    lengths = [y[2] for y in x]
    algs = [y[3] for y in x]
    particless = [y[4] for y in x]
    stepss = [y[5] for y in x]
    return pd.DataFrame({'system': systems,
                         'model': models,
                         'length': lengths,
                         'alg': algs,
                         'particles' : particless,
                         'steps': stepss})

def style(system):
    if system == 'MonadBayes':
        return 'ro'
    elif system == 'Anglican':
        return 'bs'
    else:
        return 'gX'

models = ["LR", "HMM", "LDA"]
algs = ["MH", "SMC", "RMSMC"]
systems = ["MonadBayes", "Anglican", "WebPPL"]


# plot execution time vs. dataset size

benchmarks = pd.read_csv("speed-length.csv")
results = unpack_names(benchmarks["Name"])
results["time"] = benchmarks["Mean"]
results["timeLB"] = benchmarks["MeanLB"]
results["timeUB"] = benchmarks["MeanUB"]



mhsteps = 100
smcsize = 100
rmsize, rmsteps = 10, 1

fig, subplots = plt.subplots(nrows = len(models), ncols = len(algs), figsize=(12, 8))
lines = []
for i in range(len(models)):
    model = models[i]
    for j in range(len(algs)):
        alg = algs[j]
        subplot = subplots[i,j]
        data = results.loc[(results['model'] == model) & (results['alg'] == alg)]
        if alg == 'MH':
            data = data.loc[data['steps'] == mhsteps]
        elif alg == 'SMC':
            data = data.loc[data['particles'] == smcsize]
        else:
            data = data.loc[(data['steps'] == rmsteps) & (data['particles'] == rmsize)]
        for system in systems:
            t = data.loc[data['system'] == system]
            xs = t['length']
            ys = t['time']
            if model == 'LDA':
                # LDA has 5 documents
                xs = xs * 5
            line, = subplot.plot(xs, ys, style(system), label=system)
            lines.append((line, system))
        if i == len(models) - 1:
            subplot.set_xlabel("Dataset size")
        if j == 0:
            subplot.set_ylabel("Execution time [s]")

pad = 5
algnames = ['MH' + str(mhsteps), 'SMC' + str(smcsize), 'RMSMC' + str(rmsize) + '-' + str(rmsteps)]
for ax, col in zip(subplots[0], algnames):
    ax.annotate(col, xy=(0.5, 1), xytext=(0, pad),
                xycoords='axes fraction', textcoords='offset points',
                size='large', ha='center', va='baseline')

for ax, row in zip(subplots[:,0], models):
    ax.annotate(row, xy=(0, 0.5), xytext=(-ax.yaxis.labelpad - pad, 0),
                xycoords=ax.yaxis.label, textcoords='offset points',
                size='large', ha='right', va='center')

a,b = zip(*lines[:3])
b = ("Ours", b[1], b[2])
plt.figlegend(a, b, 'upper right')
plt.savefig("length.pdf")


# plot execution time vs. # samples

benchmarks = pd.read_csv("speed-samples.csv")
results = unpack_names(benchmarks["Name"])
results["time"] = benchmarks["Mean"]
results["timeLB"] = benchmarks["MeanLB"]
results["timeUB"] = benchmarks["MeanUB"]

lrlength = 50
hmmlength = 20
ldalength = 10
rmparticles = 10
fig, subplots = plt.subplots(nrows = len(models), ncols = len(algs), figsize=(12, 8))
lines = []
for i in range(len(models)):
    model = models[i]
    for j in range(len(algs)):
        alg = algs[j]
        subplot = subplots[i,j]
        data = results.loc[(results['model'] == model) & (results['alg'] == alg)]
        if model == 'LR':
            data = data.loc[data['length'] == lrlength]
        elif model == 'HMM':
            data = data.loc[data['length'] == hmmlength]
        else:
            data = data.loc[data['length'] == ldalength]
        for system in systems:
            t = data.loc[data['system'] == system]
            if alg == 'MH':
                xs = t['steps']
                if i == len(models) - 1:
                    subplot.set_xlabel("Number of steps")
            elif alg == 'SMC':
                xs = t['particles']
                if i == len(models) - 1:
                    subplot.set_xlabel("Number of particles")
            else:
                t = t.loc[t['particles'] == rmparticles]
                xs = t['steps']
                if i == len(models) - 1:
                    subplot.set_xlabel("Number of rejuvenation steps")
            ys = t['time']
            line, = subplot.plot(xs, ys, style(system), label=system)
            lines.append((line, system))
        if j == 0:
            subplot.set_ylabel("Execution time [s]")

pad = 5
algnames = ['MH', 'SMC', 'RMSMC' + str(rmsize)]
for ax, col in zip(subplots[0], algnames):
    ax.annotate(col, xy=(0.5, 1), xytext=(0, pad),
                xycoords='axes fraction', textcoords='offset points',
                size='large', ha='center', va='baseline')

modelnames = ["LR" + str(lrlength), "HMM" + str(hmmlength), "LDA" + str(ldalength*5)]
for ax, row in zip(subplots[:,0], modelnames):
    ax.annotate(row, xy=(0, 0.5), xytext=(-ax.yaxis.labelpad - pad, 0),
                xycoords=ax.yaxis.label, textcoords='offset points',
                size='large', ha='right', va='center')

a,b = zip(*lines[:3])
b = ("Ours", b[1], b[2])
plt.figlegend(a, b, 'upper right')
plt.savefig("samples.pdf")
