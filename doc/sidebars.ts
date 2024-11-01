import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {

  tutorialsSidebar: [
    {
      type: 'category',
      label: 'Tutorials',
      items: [
        { type: 'doc', id: 'tutorials/getting-started/README', label: 'Getting Started' },
        { type: 'doc', id: 'tutorials/provider-explicit/README', label: 'Provider Explicit' },
      ],
    },
  ],

  guidesSidebar: [
    {
      type: 'category',
      label: 'Guides',
      items: [
        { type: 'doc', id: 'guides/packaging/README', label: 'Packaging' },
      ],
    },
  ],

  referenceSidebar: [
    {
      type: 'category',
      label: 'Reference',
      items: [
        { type: 'doc', id: 'reference/odoc', label: 'OCaml Packages' },
        { type: 'doc', id: 'reference/glossary', label: 'Glossary' },
        { type: 'doc', id: 'reference/hello_world', label: 'Hello World' },
      ],
    },
  ],

  explanationSidebar: [
    {
      type: 'category',
      label: 'Explanation',
      items: [
        { type: 'doc', id: 'explanation/README', label: 'Introduction' },
        { type: 'doc', id: 'explanation/lookup-strategy/README', label: 'Lookup Strategy' },
      ],
    },
  ],
};

export default sidebars;
