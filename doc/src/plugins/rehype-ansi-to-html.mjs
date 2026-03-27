import AnsiToHtml from 'ansi-to-html';
import { visit } from 'unist-util-visit';
import { fromHtml } from 'hast-util-from-html';

export default function rehypeAnsiToHtml() {
  const converter = new AnsiToHtml();

  return (tree) => {
    visit(tree, 'element', (node, _index, parent) => {
      if (
        node.tagName !== 'code' ||
        !parent ||
        parent.tagName !== 'pre' ||
        !node.properties?.className?.includes('language-ansi')
      ) {
        return;
      }

      // Collect all text content from children.
      const raw = collectText(node);
      const html = converter.toHtml(raw);

      node.properties.className = undefined;
      // Parse HTML into proper hast nodes (MDX doesn't support raw nodes).
      const fragment = fromHtml(html, { fragment: true });
      node.children = fragment.children;
    });
  };
}

function collectText(node) {
  if (node.type === 'text') return node.value;
  if (node.children) return node.children.map(collectText).join('');
  return '';
}
