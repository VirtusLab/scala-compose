import React from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import TitleSection from '../components/TitleSection';
import Section from '../components/Section';
import allFeatures from '../components/features';

const Index = (props) => {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout title="Scripting" description="Page describing why Scala CLI is good for prototyping / experimenting / reproducing  Scala.">
      <div className="container padding--sm content">
        <TitleSection><h1>prototyping / experimenting / reproducing with Scala CLI</h1></TitleSection>

        <Section>
          <p>TODO: describe why Scala CLI is a perfect for prototyping / experimenting / reproducing Plus some image?</p>
        </Section>

        {allFeatures().filter(f => f.props.prototyping)}
      </div>
    </Layout>
  );
};

export default Index;
