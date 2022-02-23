from processor import Processor
import argparse
parser = argparse.ArgumentParser(description='Tex-Like PreProcessor')
parser.add_argument(
    '--input', '-i',
    type=str, 
    help='input file', 
    required=True)
parser.add_argument(
    '--output', '-o',
    type=str, 
    help='output file', 
    required=True)
parser.add_argument(
    '--load', '-l',
    type=str, 
    help='load file for macro definitions')
parser.add_argument(
    '--verbose', '-v',
    type=int, default=0,
    help='verbose 0(off, default) or 1(on)')
args = parser.parse_args()

ifilename = args.input
ofilename = args.output
lfilename = args.load
verbose = args.verbose
if lfilename is not None:
    lfile = open(lfilename, 'r')
    configs = lfile.read()
    lfile.close()
    loadProcessor = Processor(configs, verbose=verbose)
    _ = loadProcessor.scan()
    Processor.globalMacros = {k: v for k, v in loadProcessor.macros.items()}
ifile = open(ifilename, 'r')
itext = ifile.read()
ifile.close()
processor = Processor(itext, verbose=verbose)
_ = processor.scan()
ofile = open(ofilename, 'w+')
ofile.write(processor.result)
ofile.close()

