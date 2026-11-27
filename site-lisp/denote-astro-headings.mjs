// Resolve exported heading Markdown with the same processor as yejun.dev.
import { createRequire } from 'node:module';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const require = createRequire(resolve(process.argv[2], 'package.json'));
const { createMarkdownProcessor } = await import(pathToFileURL(require.resolve('@astrojs/markdown-remark')));
const { default: rehypeRaw } = await import(pathToFileURL(require.resolve('rehype-raw')));
const processor = await createMarkdownProcessor({
  syntaxHighlight: false,
  smartypants: { dashes: 'oldschool' },
  rehypePlugins: [rehypeRaw],
});
let markdown = '';
process.stdin.setEncoding('utf8');
for await (const chunk of process.stdin) markdown += chunk;
const { metadata } = await processor.render(markdown);
process.stdout.write(JSON.stringify(metadata.headings.map(heading => heading.slug)));
